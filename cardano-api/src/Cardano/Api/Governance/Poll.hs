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
    AsType(..),

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

import           Cardano.Prelude hiding (poll)

import           Data.List (lookup)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Builder as Text.Builder

import           Cardano.Api.HasTypeProxy
import           Cardano.Api.Hash
import           Cardano.Api.SerialiseCBOR
import           Cardano.Api.SerialiseRaw
import           Cardano.Api.SerialiseTextEnvelope
import           Cardano.Api.SerialiseUsing
import           Cardano.Api.TxMetadata

import           Cardano.Ledger.Crypto (HASH, StandardCrypto, VRF)
import           Cardano.Ledger.Keys (KeyRole(..), SignedDSIGN, SignKeyDSIGN,
                   SignKeyVRF, VKey(..), VerKeyVRF, signedDSIGN, verifySignedDSIGN)

import qualified Cardano.Crypto.DSIGN as DSIGN
import qualified Cardano.Crypto.Hash as Hash
import           Cardano.Crypto.Util (SignableRepresentation(..))
import qualified Cardano.Crypto.VRF as VRF


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
  deriving Show

instance HasTextEnvelope GovernancePoll where
   textEnvelopeType _ = "GovernancePoll"

instance HasTypeProxy GovernancePoll where
    data AsType GovernancePoll = AsGovernancePoll
    proxyToAsType _ = AsGovernancePoll

instance SerialiseAsCBOR GovernancePoll where
    serialiseToCBOR =
      error "not implemented"

    deserialiseFromCBOR AsGovernancePoll _bs =
      error "not implemented"


--  ----------------------------------------------------------------------------
-- Governance Poll Hash
--

newtype instance Hash GovernancePoll =
    GovernancePollHash (Hash.Hash (HASH StandardCrypto) GovernancePoll)
  deriving stock (Eq, Ord)
  deriving (Show, IsString) via UsingRawBytesHex (Hash GovernancePoll)

instance SerialiseAsRawBytes (Hash GovernancePoll) where
    serialiseToRawBytes =
      error "not implemented"

    deserialiseFromRawBytes (AsHash AsGovernancePoll) _bs =
      error "not implemented"

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
  deriving Show

instance HasTypeProxy GovernancePollAnswer where
    data AsType GovernancePollAnswer = AsGovernancePollAnswer
    proxyToAsType _ = AsGovernancePollAnswer

instance SignableRepresentation GovernancePollAnswer where
    getSignableRepresentation =
      error "not implemented"

instance SerialiseAsCBOR GovernancePollAnswer where
    serialiseToCBOR =
      error "not implemented"

    deserialiseFromCBOR AsGovernancePollAnswer _bs =
      error "not implemented"


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
  deriving Show

instance HasTypeProxy GovernancePollWitness where
    data AsType GovernancePollWitness = AsGovernancePollWitness
    proxyToAsType _ = AsGovernancePollWitness

instance SerialiseAsCBOR GovernancePollWitness where
    serialiseToCBOR =
      error "not implemented"

    deserialiseFromCBOR AsGovernancePollWitness _bs =
      error "not implemented"

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
        VRF.verifyVRF () vk answer (panic "unused", proof)
      GovernancePollWitnessColdKey vk sig ->
        verifySignedDSIGN vk answer sig
