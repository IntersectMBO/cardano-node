{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- | An API for driving on-chain poll for SPOs.
--
-- Polls are done on-chain through transaction metadata and authenticated via
-- stake pool credentials (either VRF public key or Ed25519 cold key).
--
-- The goal is to gather opinions on governance matters such as protocol
-- parameters updates. This standard is meant to be an inclusive interim
-- solution while the work on a larger governance framework such as
-- CIP-1694 continues.
module Cardano.Api.Governance.Poll(
    -- * Type Proxies
    AsType (..),
    Hash (..),

    -- * Types
    GovernancePoll (..),
    GovernancePollAnswer (..),

    -- * Errors
    GovernancePollError (..),
    renderGovernancePollError,

    -- * Functions
    hashGovernancePoll,
    verifyPollAnswer,
  ) where

import           Control.Arrow (left)
import           Control.Monad (foldM, when)
import           Data.Either.Combinators (maybeToRight)
import           Data.Function ((&))
import qualified Data.Map.Strict as Map
import           Data.String (IsString (..))
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Builder as Text.Builder
import           Data.Word (Word64)
import           Formatting (build, sformat)
import qualified Prettyprinter as PP

import           Cardano.Api.Eras
import           Cardano.Api.Hash
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.Keys.Shelley
import           Cardano.Api.Pretty
import           Cardano.Api.SerialiseCBOR
import           Cardano.Api.SerialiseRaw
import           Cardano.Api.SerialiseTextEnvelope
import           Cardano.Api.SerialiseUsing
import           Cardano.Api.Tx
import           Cardano.Api.TxBody
import           Cardano.Api.TxMetadata
import           Cardano.Api.Utils

import           Cardano.Binary (DecoderError (..))
import           Cardano.Ledger.Crypto (HASH, StandardCrypto)

import           Cardano.Crypto.Hash (hashFromBytes, hashToBytes, hashWith)
import qualified Cardano.Crypto.Hash as Hash

-- | Associated metadata label as defined in CIP-0094
pollMetadataLabel :: Word64
pollMetadataLabel = 94

-- | Key used to identify the question in a poll metadata object
pollMetadataKeyQuestion :: TxMetadataValue
pollMetadataKeyQuestion = TxMetaNumber 0

-- | Key used to identify the possible answers in a poll metadata object
pollMetadataKeyAnswers :: TxMetadataValue
pollMetadataKeyAnswers = TxMetaNumber 1

-- | Key used to identify the question hash in a poll metadata object
pollMetadataKeyPoll :: TxMetadataValue
pollMetadataKeyPoll = TxMetaNumber 2

-- | Key used to identify a chosen answer in a poll metadata object
pollMetadataKeyChoice :: TxMetadataValue
pollMetadataKeyChoice = TxMetaNumber 3

-- | Key used to identify the optional nonce in a poll metadata object
pollMetadataKeyNonce :: TxMetadataValue
pollMetadataKeyNonce = TxMetaText "_"

-- ----------------------------------------------------------------------------
-- Governance Poll
--

-- | A governance poll declaration meant to be created by one of the genesis
-- delegates and directed towards SPOs.
--
-- A poll is made of a question and some pre-defined answers to chose from.
-- There's an optional nonce used to make poll unique (as things down the line
-- are based on their hashes) if the same question/answers need to be asked
-- multiple times.
data GovernancePoll = GovernancePoll
    { govPollQuestion :: Text
      -- ^ A question as a human readable text; the text can be arbitrarily large.
    , govPollAnswers :: [Text]
      -- ^ Answers as human readable texts; their positions are used for answering.
    , govPollNonce :: Maybe Word
      -- ^ An optional nonce to make the poll unique if needs be.
    }
  deriving (Show, Eq)

instance HasTextEnvelope GovernancePoll where
   textEnvelopeType _ = "GovernancePoll"

instance HasTypeProxy GovernancePoll where
    data AsType GovernancePoll = AsGovernancePoll
    proxyToAsType _ = AsGovernancePoll

instance AsTxMetadata GovernancePoll where
    asTxMetadata GovernancePoll{govPollQuestion, govPollAnswers, govPollNonce} =
      makeTransactionMetadata $ Map.fromList
        [ ( pollMetadataLabel
          , TxMetaMap $
           [ ( pollMetadataKeyQuestion, metaTextChunks govPollQuestion )
           , ( pollMetadataKeyAnswers, TxMetaList (metaTextChunks <$> govPollAnswers) )
           ] ++
           case govPollNonce of
             Nothing -> []
             Just nonce ->
               [ ( pollMetadataKeyNonce, TxMetaNumber (toInteger nonce) )
               ]
          )
        ]

instance SerialiseAsCBOR GovernancePoll where
    serialiseToCBOR =
      serialiseToCBOR . asTxMetadata

    deserialiseFromCBOR AsGovernancePoll bs = do
      metadata <- deserialiseFromCBOR AsTxMetadata bs
      withNestedMap lbl pollMetadataLabel metadata $ \values ->
        GovernancePoll
          -- Question
          <$> ( let key = pollMetadataKeyQuestion in case lookup key values of
                  Just x  ->
                    expectTextChunks (fieldPath lbl key) x
                  Nothing ->
                    Left $ missingField (fieldPath lbl key)
              )
          -- Answers
          <*> ( let key = pollMetadataKeyAnswers in case lookup key values of
                  Just (TxMetaList xs) ->
                    traverse (expectTextChunks (fieldPath lbl key)) xs
                  Just _  ->
                    Left $ malformedField (fieldPath lbl key) "List of Text (answers)"
                  Nothing ->
                    Left $ missingField (fieldPath lbl key)
              )
          -- Nonce (optional)
          <*> ( let key = pollMetadataKeyNonce in case lookup key values of
                  Just (TxMetaNumber nonce) ->
                    Just <$> expectWord (fieldPath lbl key) nonce
                  Nothing ->
                    pure Nothing
                  Just _  ->
                    Left $ malformedField (fieldPath lbl key) "Number (nonce)"
              )
     where
       lbl = "GovernancePoll"

--  ----------------------------------------------------------------------------
-- Governance Poll Hash
--

newtype instance Hash GovernancePoll =
    GovernancePollHash { unGovernancePollHash :: Hash.Hash (HASH StandardCrypto) GovernancePoll }
  deriving stock (Eq, Ord)
  deriving (Show, IsString) via UsingRawBytesHex (Hash GovernancePoll)

instance SerialiseAsRawBytes (Hash GovernancePoll) where
    serialiseToRawBytes =
      hashToBytes . unGovernancePollHash

    deserialiseFromRawBytes (AsHash AsGovernancePoll) bs =
      maybeToRight (SerialiseAsRawBytesError "Unable to deserialise Hash(GovernancePoll)") $
        GovernancePollHash <$> hashFromBytes bs

hashGovernancePoll :: GovernancePoll -> Hash GovernancePoll
hashGovernancePoll =
  GovernancePollHash . hashWith @(HASH StandardCrypto) serialiseToCBOR


-- ----------------------------------------------------------------------------
-- Governance Poll Answer
--

-- | An (unauthenticated) answer to a poll from an SPO referring to a poll by
-- hash digest value.
data GovernancePollAnswer = GovernancePollAnswer
    { govAnsPoll :: Hash GovernancePoll
      -- ^ The target poll
    , govAnsChoice :: Word
      -- ^ The (0-based) index of the chosen answer from that poll
    }
  deriving (Show, Eq)

instance HasTypeProxy GovernancePollAnswer where
    data AsType GovernancePollAnswer = AsGovernancePollAnswer
    proxyToAsType _ = AsGovernancePollAnswer

instance AsTxMetadata GovernancePollAnswer where
    asTxMetadata GovernancePollAnswer{govAnsPoll, govAnsChoice} =
      makeTransactionMetadata $ Map.fromList
        [ ( pollMetadataLabel
          , TxMetaMap
           [ ( pollMetadataKeyPoll, TxMetaBytes (serialiseToRawBytes govAnsPoll) )
           , ( pollMetadataKeyChoice, TxMetaNumber (toInteger govAnsChoice) )
           ]
          )
        ]

instance SerialiseAsCBOR GovernancePollAnswer where
    serialiseToCBOR =
      serialiseToCBOR . asTxMetadata

    deserialiseFromCBOR AsGovernancePollAnswer bs = do
      metadata <- deserialiseFromCBOR AsTxMetadata bs
      withNestedMap lbl pollMetadataLabel metadata $ \values ->
        GovernancePollAnswer
          -- Poll
          <$> ( let key = pollMetadataKeyPoll in case lookup key values of
                  Nothing ->
                    Left $ missingField (fieldPath lbl key)
                  Just x  ->
                    expectHash key x
              )
          -- Answer
          <*> ( let key = pollMetadataKeyChoice in case lookup key values of
                  Just (TxMetaNumber n) ->
                    expectWord (fieldPath lbl key) n
                  Just _  ->
                    Left $ malformedField (fieldPath lbl key) "Number (answer index)"
                  Nothing ->
                    Left $ missingField (fieldPath lbl key)
              )
     where
       lbl = "GovernancePollAnswer"

       expectHash key value =
         case value of
           TxMetaBytes bytes ->
             left
               (DecoderErrorCustom (fieldPath lbl key) . Text.pack . unSerialiseAsRawBytesError)
               (deserialiseFromRawBytes (AsHash AsGovernancePoll) bytes)
           _ ->
             Left (malformedField (fieldPath lbl key) "Bytes (32 bytes hash digest)")


-- ----------------------------------------------------------------------------
-- Governance Poll Verification
--

data GovernancePollError
  = ErrGovernancePollMismatch GovernancePollMismatchError
  | ErrGovernancePollNoAnswer
  | ErrGovernancePollUnauthenticated
  | ErrGovernancePollMalformedAnswer DecoderError
  | ErrGovernancePollInvalidAnswer GovernancePollInvalidAnswerError
  deriving Show

data GovernancePollInvalidAnswerError = GovernancePollInvalidAnswerError
  { invalidAnswerAcceptableAnswers :: [(Word, Text)]
  , invalidAnswerReceivedAnswer :: Word
  }
  deriving Show

data GovernancePollMismatchError = GovernancePollMismatchError
  { specifiedHashInAnswer :: Hash GovernancePoll
  , calculatedHashFromPoll :: Hash GovernancePoll
  }
  deriving Show

renderGovernancePollError :: GovernancePollError -> Doc Ann
renderGovernancePollError err =
  case err of
    ErrGovernancePollMismatch mismatchErr ->
      PP.vsep
      [ "Answer's poll doesn't match provided poll (hash mismatch)."
      , PP.indent 2 $ PP.vsep
        [ "Hash specified in answer:  " <> pretty (show (specifiedHashInAnswer mismatchErr))
        , "Hash calculated from poll: " <> pretty (show (calculatedHashFromPoll mismatchErr))
        ]
      ]
    ErrGovernancePollNoAnswer ->
      "No answer found in the provided transaction's metadata."
    ErrGovernancePollUnauthenticated -> mconcat
      [ "No (valid) signatories found for the answer. "
      , "Signatories MUST be specified as extra signatories on the transaction "
      , "and cannot be mere payment keys."
      ]
    ErrGovernancePollMalformedAnswer decoderErr ->
      "Malformed metadata; couldn't deserialise answer: " <> pretty (sformat build decoderErr)
    ErrGovernancePollInvalidAnswer invalidAnswer ->
      PP.vsep
        [ mconcat
          [ "Invalid answer ("
          , pretty (invalidAnswerReceivedAnswer invalidAnswer)
          , ") not part of the poll."
          ]
        , "Accepted answers:"
        , PP.vsep
          [ pretty ix <> " → " <> pretty answer
          | (ix, answer) <- invalidAnswerAcceptableAnswers invalidAnswer
          ]
        ]

-- | Verify a poll against a given transaction and returns the signatories
-- (verification key only) when valid.
--
-- Note: signatures aren't checked as it is assumed to have been done externally
-- (the existence of the transaction in the ledger provides this guarantee).
verifyPollAnswer
  :: GovernancePoll
  -> InAnyCardanoEra Tx
  -> Either GovernancePollError [Hash PaymentKey]
verifyPollAnswer poll (InAnyCardanoEra _era (getTxBody -> TxBody body)) = do
  answer <- extractPollAnswer (txMetadata body)
  answer `hasMatchingHash` hashGovernancePoll poll
  answer `isAmongAcceptableChoices` govPollAnswers poll
  extraKeyWitnesses (txExtraKeyWits body)
 where
  extractPollAnswer = \case
      TxMetadataNone ->
        Left ErrGovernancePollNoAnswer
      TxMetadataInEra _era metadata ->
        left ErrGovernancePollMalformedAnswer $
          deserialiseFromCBOR AsGovernancePollAnswer (serialiseToCBOR metadata)

  hasMatchingHash answer calculatedHashFromPoll = do
    let specifiedHashInAnswer = govAnsPoll answer
    when (calculatedHashFromPoll /= specifiedHashInAnswer) $
      Left $ ErrGovernancePollMismatch $
        GovernancePollMismatchError
          { specifiedHashInAnswer
          , calculatedHashFromPoll
          }

  isAmongAcceptableChoices answer answers =
    when (govAnsChoice answer >= fromIntegral (length answers)) $ do
      let invalidAnswerReceivedAnswer = govAnsChoice answer
      let invalidAnswerAcceptableAnswers = zip [0..] answers
      Left $ ErrGovernancePollInvalidAnswer $ GovernancePollInvalidAnswerError
        { invalidAnswerReceivedAnswer
        , invalidAnswerAcceptableAnswers
        }

  extraKeyWitnesses = \case
    TxExtraKeyWitnesses _era witnesses ->
      pure witnesses
    TxExtraKeyWitnessesNone ->
      Left ErrGovernancePollUnauthenticated

-- ----------------------------------------------------------------------------
-- Decoder Helpers
--

withNestedMap
  :: Text
  -> Word64
  -> TxMetadata
  -> ([(TxMetadataValue, TxMetadataValue)] -> Either DecoderError a)
  -> Either DecoderError a
withNestedMap lbl topLevelLabel (TxMetadata m) continueWith =
  case Map.lookup topLevelLabel m of
    Just (TxMetaMap values) ->
      continueWith values
    Nothing ->
      Left $ DecoderErrorCustom lbl
        ("missing expected label: " <> textShow topLevelLabel)
    Just _ ->
      Left $ DecoderErrorCustom lbl
        "malformed data; expected a key:value map"

expectTextChunks :: Text -> TxMetadataValue -> Either DecoderError Text
expectTextChunks lbl value =
  case value of
    TxMetaList xs ->
      foldM expectText mempty xs
        & maybe
            (Left (malformedField (lbl <> "[i]") "Text"))
            (Right . Text.Lazy.toStrict . Text.Builder.toLazyText)
    _ ->
      Left (malformedField lbl "List<Text>")
 where
  expectText acc x =
    case x of
      TxMetaText txt -> Just (acc <> Text.Builder.fromText txt)
      _ -> Nothing

expectWord :: Text -> Integer -> Either DecoderError Word
expectWord lbl n
  | n >= 0 && n < toInteger (maxBound :: Word) =
      pure (fromInteger n)
  | otherwise =
      Left $ DecoderErrorCustom lbl
        "invalid number; must be non-negative word"

missingField :: Text -> DecoderError
missingField lbl =
  DecoderErrorCustom lbl
    "missing mandatory field"

malformedField :: Text -> Text -> DecoderError
malformedField lbl hint =
  DecoderErrorCustom lbl
    ("malformed field; must be: " <> hint)

fieldPath
  :: Text
    -- ^ Label
  -> TxMetadataValue
    -- ^ Field key
  -> Text
fieldPath lbl (TxMetaNumber i) = lbl <> "." <> textShow i
fieldPath lbl (TxMetaText t) = lbl <> "." <> t
fieldPath lbl _ = lbl <> ".?"
