{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

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
    GovernancePollWitness (..),

    -- * Errors
    GovernancePollError (..),
    renderGovernancePollError,

    -- * Functions
    hashGovernancePoll,
    signPollAnswerWith,
    verifyPollAnswer,
  ) where

import           Control.Arrow (left)
import           Control.Monad (foldM, unless, when)
import           Data.Either.Combinators (maybeToRight)
import           Data.Function ((&))
import qualified Data.Map.Strict as Map
import           Data.String (IsString (..))
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Builder as Text.Builder
import           Data.Word (Word64)

import           Cardano.Api.Hash
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.SerialiseCBOR
import           Cardano.Api.SerialiseRaw
import           Cardano.Api.SerialiseTextEnvelope
import           Cardano.Api.SerialiseUsing
import           Cardano.Api.TxMetadata
import           Cardano.Api.Utils

import           Cardano.Binary (DecoderError (..))
import           Cardano.Ledger.Crypto (HASH, StandardCrypto, VRF)
import           Cardano.Ledger.Keys (KeyRole (..), SignKeyDSIGN, SignKeyVRF, SignedDSIGN,
                   VKey (..), VerKeyVRF, signedDSIGN, verifySignedDSIGN)

import qualified Cardano.Crypto.DSIGN as DSIGN
import           Cardano.Crypto.Hash (hashFromBytes, hashToBytes, hashWith)
import qualified Cardano.Crypto.Hash as Hash
import           Cardano.Crypto.Util (SignableRepresentation (..))
import qualified Cardano.Crypto.VRF as VRF

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

-- | Key used to identify a VRF proof witness in a poll metadata object
pollMetadataKeyWitnessVRF :: TxMetadataValue
pollMetadataKeyWitnessVRF = TxMetaNumber 4

-- | Key used to identify a cold key witness in a poll metadata object
pollMetadataKeyWitnessColdKey :: TxMetadataValue
pollMetadataKeyWitnessColdKey = TxMetaNumber 5

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

instance SignableRepresentation GovernancePollAnswer where
    getSignableRepresentation =
      hashToBytes . hashWith @(HASH StandardCrypto) (serialiseToCBOR . asTxMetadata)

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
-- Governance Poll Witness
--

-- | A governance poll witness, effectively authenticating a
-- 'GovernancePollAnswer' using either a VRF proof or a digital signature from a
-- cold key.
data GovernancePollWitness
    = GovernancePollWitnessVRF
        (VerKeyVRF StandardCrypto)
        (VRF.CertVRF (VRF StandardCrypto))
    | GovernancePollWitnessColdKey
        (VKey 'Witness StandardCrypto)
        (SignedDSIGN StandardCrypto GovernancePollAnswer)
  deriving (Show, Eq)

instance HasTypeProxy GovernancePollWitness where
    data AsType GovernancePollWitness = AsGovernancePollWitness
    proxyToAsType _ = AsGovernancePollWitness

instance AsTxMetadata GovernancePollWitness where
    asTxMetadata witness =
      makeTransactionMetadata $ Map.fromList
        [ ( pollMetadataLabel
          , TxMetaMap
           [ case witness of
              GovernancePollWitnessVRF vk proof ->
                ( pollMetadataKeyWitnessVRF
                , TxMetaList
                    -- NOTE (1): VRF keys are 32-byte long.
                    -- NOTE (2): VRF proofs are 80-byte long.
                    [ TxMetaBytes $ VRF.rawSerialiseVerKeyVRF vk
                    , metaBytesChunks (VRF.rawSerialiseCertVRF proof)
                    ]
                )
              GovernancePollWitnessColdKey (VKey vk) (DSIGN.SignedDSIGN sig) ->
                ( pollMetadataKeyWitnessColdKey
                , TxMetaList
                    -- NOTE (1): Ed25519 keys are 32-byte long.
                    -- NOTE (2): Ed25519 signatures are 64-byte long.
                    [ TxMetaBytes $ DSIGN.rawSerialiseVerKeyDSIGN vk
                    , TxMetaBytes $ DSIGN.rawSerialiseSigDSIGN sig
                    ]
                )
           ]
          )
        ]

instance SerialiseAsCBOR GovernancePollWitness where
    serialiseToCBOR =
      serialiseToCBOR . asTxMetadata

    deserialiseFromCBOR AsGovernancePollWitness bs = do
      metadata <- deserialiseFromCBOR AsTxMetadata bs
      withNestedMap lbl pollMetadataLabel metadata $ \values ->
        tryWitnessVRF values $
          tryColdKey values $
            Left $ missingField (fieldPath lbl (TxMetaText "{4|5}"))
     where
       lbl = "GovernancePollWitness"

       tryWitnessVRF values orElse =
         let k = pollMetadataKeyWitnessVRF in case lookup k values of
           Just (TxMetaList [TxMetaBytes vk, TxMetaList[TxMetaBytes proofHead, TxMetaBytes proofTail]]) ->
             expectJust (fieldPath lbl k) $ GovernancePollWitnessVRF
               <$> VRF.rawDeserialiseVerKeyVRF vk
               <*> VRF.rawDeserialiseCertVRF (proofHead <> proofTail)
           Just _  ->
             Left $ malformedField (fieldPath lbl k) "List"
           Nothing ->
             orElse

       tryColdKey values orElse =
         let k = pollMetadataKeyWitnessColdKey in case lookup k values of
           Just (TxMetaList [TxMetaBytes vk, TxMetaBytes sig]) ->
             expectJust (fieldPath lbl k) $ GovernancePollWitnessColdKey
               <$> fmap VKey (DSIGN.rawDeserialiseVerKeyDSIGN vk)
               <*> fmap DSIGN.SignedDSIGN (DSIGN.rawDeserialiseSigDSIGN sig)
           Just _  ->
             Left $ malformedField (fieldPath lbl k) "List"
           Nothing ->
             orElse

signPollAnswerWith
  :: GovernancePollAnswer
  -> Either (SignKeyVRF StandardCrypto) (SignKeyDSIGN StandardCrypto)
  -> GovernancePollWitness
signPollAnswerWith answer =
  either
    (\sk -> GovernancePollWitnessVRF
      (VRF.deriveVerKeyVRF sk)
      (snd $ VRF.evalVRF () answer sk)
    )
    (\sk -> GovernancePollWitnessColdKey
      (VKey (DSIGN.deriveVerKeyDSIGN sk))
      (signedDSIGN @StandardCrypto sk answer)
    )

-- ----------------------------------------------------------------------------
-- Governance Poll Verification
--

data GovernancePollError
  = ErrGovernancePollMismatch
  | ErrGovernancePollInvalidAnswer GovernancePollInvalidAnswerError
  | ErrGovernancePollInvalidWitness
  deriving Show

data GovernancePollInvalidAnswerError = GovernancePollInvalidAnswerError
  { invalidAnswerAcceptableAnswers :: [(Word, Text)]
  , invalidAnswerReceivedAnswer :: Word
  }
  deriving Show

renderGovernancePollError :: GovernancePollError -> Text
renderGovernancePollError err =
  case err of
    ErrGovernancePollMismatch ->
      "Answer's poll doesn't match provided poll (hash mismatch)."
    ErrGovernancePollInvalidAnswer invalidAnswer ->
        mconcat
          [ "Invalid answer ("
          , textShow (invalidAnswerReceivedAnswer invalidAnswer)
          , ") not part of the poll."
          , "\n"
          , "Accepted answers:"
          , "\n"
          , Text.intercalate "\n"
              [ mconcat
                  [ textShow ix
                  , " → "
                  , answer
                  ]
              | (ix, answer) <- invalidAnswerAcceptableAnswers invalidAnswer
              ]
          ]
    ErrGovernancePollInvalidWitness ->
      "Invalid witness for the answer: the proof / signature doesn't hold."

verifyPollAnswer
  :: GovernancePoll
  -> GovernancePollAnswer
  -> GovernancePollWitness
  -> Either GovernancePollError ()
verifyPollAnswer poll answer witness = do
  when (hashGovernancePoll poll /= govAnsPoll answer) $
    Left ErrGovernancePollMismatch

  when (govAnsChoice answer >= fromIntegral (length (govPollAnswers poll))) $ do
    let invalidAnswerReceivedAnswer = govAnsChoice answer
    let invalidAnswerAcceptableAnswers = zip [0..] (govPollAnswers poll)
    Left $ ErrGovernancePollInvalidAnswer $ GovernancePollInvalidAnswerError
      { invalidAnswerReceivedAnswer
      , invalidAnswerAcceptableAnswers
      }

  unless isValid $
    Left ErrGovernancePollInvalidWitness
 where
   isValid =
    case witness of
      GovernancePollWitnessVRF vk proof ->
        VRF.verifyVRF () vk answer (undefined, proof)
      GovernancePollWitnessColdKey vk sig ->
        verifySignedDSIGN vk answer sig


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

expectJust :: Text -> Maybe a -> Either DecoderError a
expectJust lbl =
  maybe
    (Left (DecoderErrorCustom lbl "malformed field(s)"))
    Right

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
