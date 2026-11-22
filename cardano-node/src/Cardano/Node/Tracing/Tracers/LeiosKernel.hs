{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | The node's Leios events, logged as the Leios prototype logs them under
-- @Consensus.LeiosKernel@, so that the Leios visualizer, the acceptance demo
-- and the dashboards read a production node like a prototype one.
module Cardano.Node.Tracing.Tracers.LeiosKernel
  ( LeiosKernelEvent (..)
  , leiosKernelFromForge
  , leiosKernelFromVoting
  , leiosKernelFromVoteState
  , leiosKernelFromFetchClient
  ) where

import           Cardano.Crypto.Hash (hashToStringAsHex)
import           Cardano.Crypto.Leios (LeiosSeatId (..))
import           Cardano.Logging
import           Ouroboros.Consensus.Block (ConvertRawHash, HeaderHash, SlotNo (..), realPointSlot)
import           Ouroboros.Consensus.Leios.Types (EndorserBlock, EndorserBlockHash, LeiosPoint (..),
                   LeiosVote (..), RankingBlockHash (..), encodeEndorserBlockSize,
                   endorserBlockHash, endorserBlockReferences)
import           Ouroboros.Consensus.Leios.VoteState (TraceLeiosVoteState (..))
import           Ouroboros.Consensus.Leios.Voting (TraceLeiosVoting (..))
import           Ouroboros.Consensus.MiniProtocol.LeiosFetch.Trace (TraceLeiosFetchClientEvent (..))
import           Ouroboros.Consensus.Node.Tracers (TraceForgeEvent (..))
import           Ouroboros.Consensus.Tracing.Render (renderHeaderHash)

import           Data.Aeson (Value (String), (.=))
import qualified Data.Aeson as Aeson
import           Data.Proxy (Proxy (..))
import qualified Data.Text as Text
import           Data.Time (NominalDiffTime)

data LeiosKernelEvent
  = LeiosBlockForged SlotNo EndorserBlock
  | LeiosBlockAnnounced Text.Text LeiosPoint
  | LeiosBlockCertified SlotNo LeiosPoint
  | LeiosBlockAcquired LeiosPoint (Maybe NominalDiffTime)
  | LeiosBlockTxsAcquired LeiosPoint (Maybe NominalDiffTime)
  | LeiosEbValidated LeiosPoint Int Int
  | LeiosVoted LeiosVote
  | LeiosNotVoted LeiosPoint Text.Text
  | LeiosCertified RankingBlockHash

-- | A forged endorser block is also announced by the ranking block forged with
-- it.
leiosKernelFromForge :: forall blk. ConvertRawHash blk => TraceForgeEvent blk -> [LeiosKernelEvent]
leiosKernelFromForge = \case
  TraceForgedEndorserBlock slot rbHash eb _ ->
    [ LeiosBlockForged slot eb
    , LeiosBlockAnnounced (renderRbHash rbHash) (MkLeiosPoint slot (endorserBlockHash eb))
    ]
  TraceForgedCertifyingBlock slot _ announcing ebHash ->
    [LeiosBlockCertified slot (MkLeiosPoint (realPointSlot announcing) ebHash)]
  _ -> []
 where
  renderRbHash :: HeaderHash blk -> Text.Text
  renderRbHash = renderHeaderHash (Proxy @blk)

leiosKernelFromVoting :: TraceLeiosVoting -> [LeiosKernelEvent]
leiosKernelFromVoting = \case
  TraceLeiosEbValidated point reapplied applied -> [LeiosEbValidated point reapplied applied]
  TraceLeiosVoted vote -> [LeiosVoted vote]
  TraceLeiosNotVoted point reason -> [LeiosNotVoted point (Text.pack (show reason))]

leiosKernelFromVoteState :: TraceLeiosVoteState -> [LeiosKernelEvent]
leiosKernelFromVoteState = \case
  TraceLeiosCertified rbHash _ -> [LeiosCertified rbHash]
  _ -> []

leiosKernelFromFetchClient :: TraceLeiosFetchClientEvent peer -> [LeiosKernelEvent]
leiosKernelFromFetchClient = \case
  TraceLeiosFetchClientBlockAcquired point age -> [LeiosBlockAcquired point age]
  TraceLeiosFetchClientBlockTxsAcquired point age -> [LeiosBlockTxsAcquired point age]
  _ -> []

instance LogFormatting LeiosKernelEvent where
  forMachine _dtal = \case
    LeiosBlockForged slot eb ->
      mconcat
        [ "kind" .= String "LeiosBlockForged"
        , "slot" .= slot
        , "hash" .= hex (endorserBlockHash eb)
        , "numTxs" .= length (endorserBlockReferences eb)
        , "ebSize" .= encodeEndorserBlockSize eb
        , "closureSize" .= sum (map snd (endorserBlockReferences eb))
        ]
    LeiosBlockAnnounced rbHash (MkLeiosPoint ebSlot ebHash) ->
      mconcat
        [ "kind" .= String "LeiosBlockAnnounced"
        , "rbHash" .= rbHash
        , "ebSlot" .= ebSlot
        , "ebHash" .= hex ebHash
        ]
    LeiosBlockCertified atSlot (MkLeiosPoint ebSlot ebHash) ->
      mconcat
        [ "kind" .= String "LeiosBlockCertified"
        , "atSlot" .= atSlot
        , "ebSlot" .= ebSlot
        , "ebHash" .= hex ebHash
        ]
    LeiosBlockAcquired point age ->
      mconcat $ ["kind" .= String "LeiosBlockAcquired", pointFields point]
        <> foldMap (\a -> ["bodyAgeSeconds" .= (realToFrac a :: Double)]) age
    LeiosBlockTxsAcquired point age ->
      mconcat $ ["kind" .= String "LeiosBlockTxsAcquired", pointFields point]
        <> foldMap (\a -> ["closureAgeSeconds" .= (realToFrac a :: Double)]) age
    LeiosEbValidated point reapplied applied ->
      mconcat
        [ "kind" .= String "LeiosEbValidated"
        , pointFields point
        , "reapplied" .= reapplied
        , "applied" .= applied
        ]
    LeiosVoted MkLeiosVote{voteAnnouncingRbHash, voteSeatId} ->
      mconcat
        [ "kind" .= String "LeiosVoted"
        , "vote" .= Aeson.object
            [ "rbHash" .= rbHex voteAnnouncingRbHash
            , "voterId" .= leiosSeatIndex voteSeatId
            ]
        ]
    LeiosNotVoted point reason ->
      mconcat ["kind" .= String "LeiosNotVoted", pointFields point, "reason" .= reason]
    LeiosCertified rbHash ->
      mconcat ["kind" .= String "LeiosCertified", "rbHash" .= rbHex rbHash]
   where
    hex :: EndorserBlockHash -> Value
    hex = String . Text.pack . hashToStringAsHex

    rbHex :: RankingBlockHash -> Value
    rbHex = String . Text.pack . hashToStringAsHex . unRankingBlockHash

    pointFields (MkLeiosPoint (SlotNo ebSlot) ebHash) =
      mconcat ["ebHash" .= hex ebHash, "ebSlot" .= ebSlot]

instance MetaTrace LeiosKernelEvent where
  namespaceFor = \case
    LeiosBlockForged{} -> Namespace [] ["BlockForged"]
    LeiosBlockAnnounced{} -> Namespace [] ["BlockAnnounced"]
    LeiosBlockCertified{} -> Namespace [] ["BlockCertified"]
    LeiosBlockAcquired{} -> Namespace [] ["BlockAcquired"]
    LeiosBlockTxsAcquired{} -> Namespace [] ["BlockTxsAcquired"]
    LeiosEbValidated{} -> Namespace [] ["EbValidated"]
    LeiosVoted{} -> Namespace [] ["Voted"]
    LeiosNotVoted{} -> Namespace [] ["NotVoted"]
    LeiosCertified{} -> Namespace [] ["Certified"]

  severityFor _ _ = Just Info

  documentFor _ = Just "A Leios event, logged as the Leios prototype logs it."

  allNamespaces =
    map (Namespace [] . (: []))
      [ "BlockForged", "BlockAnnounced", "BlockCertified", "BlockAcquired"
      , "BlockTxsAcquired", "EbValidated", "Voted", "NotVoted", "Certified" ]
