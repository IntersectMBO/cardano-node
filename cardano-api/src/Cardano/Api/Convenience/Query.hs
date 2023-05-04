{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Convenience query functions
--
module Cardano.Api.Convenience.Query (
    QueryConvenienceError(..),
    -- * Simplest query related
    queryStateForBalancedTx,
    renderQueryConvenienceError,
  ) where

import           Control.Monad.Trans.Except (runExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, left, newExceptT)
import           Data.Map.Strict (Map)
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text

import           Cardano.Api.Address
import           Cardano.Api.Certificate
import           Cardano.Api.Eras
import           Cardano.Api.IO
import           Cardano.Api.IPC
import           Cardano.Api.IPC.AnyQuery
import           Cardano.Api.Modes
import           Cardano.Api.NetworkId
import           Cardano.Api.ProtocolParameters
import           Cardano.Api.Query
import           Cardano.Api.Query.ShelleyBased
import           Cardano.Api.TxBody
import           Cardano.Api.Value

newtype QueryConvenienceError = QueryConvenienceError AllQueryErrors

renderQueryConvenienceError :: QueryConvenienceError -> Text
renderQueryConvenienceError (QueryConvenienceError e) = Text.pack $ show e

-- | A convenience function to query the relevant information, from
-- the local node, for Cardano.Api.Convenience.Construction.constructBalancedTx
queryStateForBalancedTx
  :: SocketPath
  -> NetworkId
  -> [TxIn]
  -> [Certificate]
  -> IO (Either QueryConvenienceError ( AnyUTxO
                                      , ProtocolParameters
                                      , EraHistory CardanoMode
                                      , SystemStart
                                      , Set PoolId
                                      , Map StakeCredential Lovelace
                                      )
        )
queryStateForBalancedTx socketPath networkId allTxIns certs = runExceptT $ do
  let cModeParams = CardanoModeParams $ EpochSlots 21600
      localNodeConnInfo = LocalNodeConnectInfo
                            cModeParams
                            networkId
                            socketPath
  firstExceptT QueryConvenienceError $ newExceptT
    $ executeLocalStateQueryExprAnyQuery localNodeConnInfo Nothing $ do
        AnyCardanoEra era <- determineEraExprAnyQuery cModeParams
        eInMode <- determineEraInModeAnyQuery era cModeParams
        case cardanoEraStyle era of
          LegacyByronEra -> left AllQueryEraExpectedSbe
          ShelleyBasedEra sbe -> do
            let utxoQuery = AnyQueryShelleyBasedEra $ QueryShelleyBasedEra eInMode $ QueryInShelleyBasedEra sbe
                              $ QueryUTxO (QueryUTxOByTxIn (Set.fromList allTxIns))
                pparamsQuery = AnyQueryShelleyBasedEra $ QueryShelleyBasedEra eInMode
                                 $ QueryInShelleyBasedEra sbe QueryProtocolParameters
                eraHistoryQuery = AnyQueryAnyEra $ QueryEraHistory CardanoModeIsMultiEra
                systemStartQuery = AnyQueryAnyEra QuerySystemStart
                stakePoolsQuery = AnyQueryShelleyBasedEra $ QueryShelleyBasedEra eInMode
                                    $ QueryInShelleyBasedEra sbe QueryStakePools
                stakeCreds = Set.fromList $ flip mapMaybe certs $ \case
                  StakeAddressDeregistrationCertificate cred -> Just cred
                  _ -> Nothing
                stakeDelegDepositsQuery = AnyQueryShelleyBasedEra $ QueryShelleyBasedEra eInMode
                                                                  $ QueryInShelleyBasedEra sbe
                                                                  $ QueryStakeDelegDeposits stakeCreds
            utxo <- AnyUTxO (shelleyBasedToCardanoEra sbe) <$> queryExprAnyQueryE utxoQuery
            pparams <- queryExprAnyQueryE pparamsQuery
            eraHistory <- queryExprAnyQuery eraHistoryQuery
            systemStart <- queryExprAnyQuery systemStartQuery
            stakePools <- queryExprAnyQueryE stakePoolsQuery
            stakeDelegDeposits <-
              if null stakeCreds
              then pure mempty
              else queryExprAnyQueryE stakeDelegDepositsQuery
            return (utxo, pparams, eraHistory, systemStart, stakePools, stakeDelegDeposits)
