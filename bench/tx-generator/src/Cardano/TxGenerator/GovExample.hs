-- The extensions may have been overly liberally used, though they did
-- help speed up development, as far as it got.


-- This section of extensions is mostly short syntax, and they're just
-- very helpful for putting code together in various ways.
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}


-- These two go together, and are extremely useful for constraining the
-- types of local variables or otherwise expressing them in terms of the
-- types appearing in the type signature of the top-level CAF or function
-- even apart from where they aid more advanced type-level programming.
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}


-- The precise implications of this may not be fully understood; however,
-- avoiding relying on it inadvertently seems tricky.
{-# LANGUAGE FlexibleContexts      #-}


-- This is very likely redundant. I believe RankNTypes implies it.
{-# LANGUAGE QuantifiedConstraints #-}


-- These more advanced features are large for interoperability with
-- imported code. While they don't seem so far out as standalone concepts,
-- Other People's Code (TM) that uses them is 
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}


-- PartialTypeSignatures was extremely useful during the development
-- process, even if it didn't make enough progress.
-- It appears to allow one to ask the type inferencer a question of what
-- type is inferred in the place that an underscore appears, or possibly
-- a type variable whose name begins with an underscore, which may depend
-- on an extension vs. just a single underscore alone.
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-error=partial-type-signatures #-}


-- Which record fields conflicted should have been logged.
-- It's not clear that the conflict was intentional, and it was from code
-- outside the direct influence of the project.
{-# LANGUAGE DuplicateRecordFields #-}


{- These are all pretty reasonable warning options.
 - These disablings of warnings and makings of warnings not errors
 - ("softenings") aren't needed anymore as the code now stands.
 - It could still be useful to keep these ready to reactivate in
 - the event of rapid code restructurings until it comes time to
 - do the final cleanup of the commit sequence.
 -* OPTIONS_GHC -fno-warn-incomplete-uni-patterns  *-
 -* OPTIONS_GHC -Wno-unrecognised-pragmas          *-
 -}

-- Switching these warnings on intermittently seemed to speed development
-- up slightly because compiles are still slow enough even on a
-- decent-capacity laptop that it helps to batch warning fixes.
{-# OPTIONS_GHC -Wno-error=unused-imports          #-}
{-# OPTIONS_GHC -Wno-error=unused-matches          #-}
{-# OPTIONS_GHC -Wno-error=unused-local-binds      #-}
{-# OPTIONS_GHC -Wno-error=redundant-constraints   #-}

{- These also used to be needed to pass hlint checks, but the warnings
 - have likewise since been silenced, so they're no longer needed to
 - make progress in the interim. In like fashion, it may be useful to
 - let these disabled pragmas linger until the point of final clean-up
 - of the commit sequence for intensive restructurings.
 -* HLINT ignore "Unused LANGUAGE pragma"          *-
 -* HLINT ignore "Avoid lambda using `infix`"      *-
 -}

module  Cardano.TxGenerator.GovExample where

import qualified Cardano.Api as Api
                   ( CardanoEra (..)
                   -- There was a point where IsConwayBasedEra wasn't in
                   -- CHaP that was awkward. It seemed like it would be
                   -- an appropriate type constraint for governance tasks.
                   , InAnyCardanoEra (..)
                   , IsCardanoEra (..)
                   , NetworkId (..)
                   , PaymentKey
                   , SigningKey (..)
                   , TextEnvelope (..)
                   , TextEnvelopeType (..)
                   , TxMetadataInEra (..))
import qualified Cardano.Api.Ledger as Ledger (PParams (..))
import qualified Cardano.Api.Shelley as Api
                   ( BuildTx
                   , BuildTxWith (..)
                   , ConwayEra
                   , CtxTx
                   , IsShelleyBasedEra (..)
                   , KeyWitnessInCtx (..)
                   , LedgerProtocolParameters (..)
                   , ShelleyBasedEra (..)
                   , ShelleyWitnessSigningKey (..)
                   , Tx (..)
                   , TxBody (..)
                   , TxBodyContent (..)
                   , TxBodyError (..)
                   , TxFee (..)
                   , TxId (..)
                   , TxIn (..)
                   , TxInsCollateral (..)
                   , TxIx (..)
                   , TxOut (..)
                   , TxOutValue (..)
                   , TxValidityLowerBound (..)
                   , WitCtxTxIn
                   , Witness (..)
                   , convertToLedgerProtocolParameters
                   , createAndValidateTransactionBody
                   -- There was a point where there was an attempt to use
                   -- this. Enough issues were happening at once that it
                   -- ended up falling by the wayside. Clearly, the task
                   -- absolutely requires it.
                   -- , createVotingProcedure
                   , defaultTxBodyContent
                   , defaultTxValidityUpperBound
                   , getTxId
                   , inEonForEra
                   , lovelaceToTxOutValue
                   , setTxFee
                   , setTxIns
                   , setTxInsCollateral
                   , setTxMetadata
                   , setTxOuts
                   , setTxProtocolParams
                   , setTxValidityLowerBound
                   , setTxValidityUpperBound
                   , shelleyBasedEra
                   , signShelleyTransaction
                   , toCardanoEra)


-- Unqualified imports of types need to be re-qualified before a PR.
-- Adjust line break to stylish-haskell/fourmolu/etc.
import qualified Cardano.Ledger.Api.PParams as Ledger (EraPParams (..))
import           Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Core as Ledger (upgradePParams)
import qualified Cardano.Ledger.Api.Era as Ledger (BabbageEra, ConwayEra)
import qualified Cardano.Ledger.Crypto as Crypto (Crypto (..))
import           Cardano.TxGenerator.FundQueue
                   (Fund (..), FundInEra (..), FundQueue)
import qualified Cardano.TxGenerator.FundQueue as FundQueue
                   ( emptyFundQueue
                   , fromList
                   , getFundCoin
                   , getFundKey
                   , getFundTxIn
                   , getFundWitness
                   , insertFund
                   , toList)
import qualified Cardano.TxGenerator.Setup.SigningKey as TxGen
                   (parseSigningKeyTE)
import           Cardano.TxGenerator.Types
                   ( FundSource
                   , FundToStoreList
                   , TxEnvironment (..)
                   , TxGenError (..)
                   , TxGenerator)
import qualified Cardano.TxGenerator.Utils as TxGen
                   (inputsToOutputsWithFee)
import qualified Cardano.TxGenerator.UTxO as UTxO
                   (ToUTxO, ToUTxOList, makeToUTxOList, mkUTxOVariant)

import           Control.Arrow ((&&&))
import qualified Control.Arrow as Arrow (left, right)
import qualified Control.Monad as Monad (foldM)
import           Control.Monad.Trans.State.Strict
import qualified Data.Aeson as Aeson (eitherDecodeFileStrict')
import qualified Data.Default.Class as Default (Default (..))
import           Data.Either (fromRight)
import           Data.Function ((&))
import           Data.Functor.Identity (Identity (..))
import qualified Data.List as List (foldl')
import qualified Data.Maybe as Maybe (mapMaybe)
import           Data.String (fromString)
import qualified Data.Tuple.Extra as Extra (uncurry3)

import qualified System.Exit as Exit (die)

import           Paths_tx_generator

-- It may be worth including ConwayEraOnwardsConstraints somehow.


demo :: IO ()
demo = getDataFileName "data/protocol-parameters.json" >>= demo'

demo' :: FilePath -> IO ()
demo' parametersFile = do
  protocolParameters <- either Exit.die pure =<< Aeson.eitherDecodeFileStrict' parametersFile
  let
      demoEnv :: TxEnvironment Api.ConwayEra
      demoEnv = TxEnvironment
        { txEnvNetworkId = Api.Mainnet
        , txEnvProtocolParams = protocolParameters
        , txEnvFee = Api.TxFeeExplicit Api.shelleyBasedEra 100000
        , txEnvMetadata = Api.TxMetadataNone }

  run1 <- mkRun $ generateTx demoEnv
  run2 <- mkRun $ generateTxM demoEnv
  putStrLn . ("Are run results identical? " ++) . show $
    FundQueue.toList run1 == FundQueue.toList run2
  where
    mkRun :: Generator (Either TxGenError (Api.Tx era)) -> IO FundQueue
    mkRun = Extra.uncurry3 Monad.foldM . (, fundQueue, [1..10]) . work
    fundQueue :: FundQueue
    fundQueue = FundQueue.emptyFundQueue `FundQueue.insertFund` genesisFund
    genesisFund :: Fund = Fund $ Api.InAnyCardanoEra Api.ConwayEra fundInEra
      where
        fundInEra :: FundInEra Api.ConwayEra
        fundInEra  = FundInEra
          { _fundTxIn = genesisTxIn
          , _fundVal = genesisValue
          , _fundWitness = Api.KeyWitness Api.KeyWitnessForSpending
          , _fundSigningKey = Just signingKey }
    work :: Generator (Either TxGenError (Api.Tx era))
         -> FundQueue
         -> Int
         -> IO FundQueue
    work pureGenerator generatorState counter = do
      putStrLn $ "running tx-generator. Iteration : " ++ show counter
      let (res, newState) = runState pureGenerator generatorState
      case res of
        Right tx -> print tx
        Left err -> print err
      return newState

signingKey :: Api.SigningKey Api.PaymentKey
signingKey = fromRight (error "signingKey: parseError") $ TxGen.parseSigningKeyTE keyData
  where
    keyData = Api.TextEnvelope { teType = Api.TextEnvelopeType "GenesisUTxOSigningKey_ed25519"
              , teDescription = fromString "Genesis Initial UTxO Signing Key"
              , teRawCBOR = "X \vl1~\182\201v(\152\250A\202\157h0\ETX\248h\153\171\SI/m\186\242D\228\NAK\182(&\162"}

drepSigningKey :: Api.SigningKey Api.PaymentKey
drepSigningKey = fromRight (error "drepSigningKey: parseError") $ TxGen.parseSigningKeyTE keyData
  where
    keyData = Api.TextEnvelope { teType = Api.TextEnvelopeType "DRepSigningKey_ed25519"
                           , teDescription = fromString "Delegate Representative Signing Key"
                           -- This is actually the CBOR as it appeared
                           -- in the JSON file and needs conversion to raw CBOR.
                           , teRawCBOR = "5820ac0757312cf883baa809d8cf6c3c48e86acc70db9c6eb5511666c8b128d9020a" }

genesisTxIn :: Api.TxIn
genesisValue :: Api.TxOutValue Api.ConwayEra

(genesisTxIn, genesisValue) =
  ( Api.TxIn "900fc5da77a0747da53f7675cbb7d149d46779346dea2f879ab811ccc72a2162" (Api.TxIx 0)
  , Api.lovelaceToTxOutValue Api.ShelleyBasedEraConway $ Coin 90000000000000
  )


type Generator = State FundQueue


-- Need to ask Carlos or Aniket what anchors are about.
-- The particular issue is what could be substituted for the URL if it
-- turns out fake ones like I used earlier error out.
-- Cardano.Api.Governance.Actions.createAnchor
--         :: Url -> ByteString -> Anchor StandardCrypto
-- There's a lingering undefined here, but it's worth keeping because
-- this is at least meant to push directly towards issuing votes.
-- There was a not usefully developed stub function here:
-- localGenVote :: forall era .  Api.ConwayEraOnwards era -> Vote -> IO ()
-- {- ConwayEraOnwardsConstraints era => -}
-- Its contents weren't meaningful enough to preserve, but this comment
-- may be informative about the direction development ended up going afoul in.

mkSignedTx :: forall era . ()
  => Api.IsShelleyBasedEra era
  => Api.LedgerProtocolParameters era
  -> [Fund]
  -> (Api.TxInsCollateral era, [Fund])
  -> Api.TxFee era
  -> Api.TxMetadataInEra era
  -> [Api.TxOut Api.CtxTx era]
  -> Either Api.TxBodyError (Api.Tx era, Api.TxId)
mkSignedTx
     ledgerParameters
     inFunds
     (collateral, collFunds)
     fee
     metadata
     -- Erroring like this may not be the most graceful no function to pass.
     outputs = flip (Api.inEonForEra $ eraErr "Unsupported era")
       (Api.toCardanoEra (Api.shelleyBasedEra :: Api.ShelleyBasedEra era))
       \eonEra ->
           Arrow.right (flip (Api.signShelleyTransaction eonEra) signingKeys
                           &&& Api.getTxId)
               (mkTxBody ledgerParameters
                         inFunds
                         (collateral, collFunds)
                         fee
                         metadata
                         outputs)
  where
    eraErr eraStr = error $ "mkTxBody: unexpected era " <> eraStr
    signingKeys :: [Api.ShelleyWitnessSigningKey]
    signingKeys = map Api.WitnessPaymentKey allKeys
    allKeys :: [Api.SigningKey Api.PaymentKey]
    allKeys = Maybe.mapMaybe FundQueue.getFundKey $ inFunds ++ collFunds


mkTxBody :: forall era .  ()
  => Api.IsShelleyBasedEra era
  => Api.LedgerProtocolParameters era
  -> [Fund]
  -> (Api.TxInsCollateral era, [Fund])
  -> Api.TxFee era
  -> Api.TxMetadataInEra era
  -> [Api.TxOut Api.CtxTx era]
  -> Either Api.TxBodyError (Api.TxBody era)
mkTxBody
     ledgerParameters
     inFunds
     (collateral, collFunds)
     fee
     metadata
     outputs
  -- The graceless method was re-applied until it no longer enabled progress.
  = flip (Api.inEonForEra $ eraErr "Unsupported era") cardanoEra $
       flip Api.createAndValidateTransactionBody bodyContent
  where
    bodyContent :: Api.TxBodyContent Api.BuildTx era
    bodyContent = mkTxBodyContent ledgerParameters
                         inFunds
                         (collateral, collFunds)
                         fee
                         metadata
                         outputs
    cardanoEra :: Api.CardanoEra era
    cardanoEra = Api.toCardanoEra shelleyBasedEra
    shelleyBasedEra :: Api.ShelleyBasedEra era
    shelleyBasedEra = Api.shelleyBasedEra
    eraErr eraStr = error $ "mkTxBody: unexpected era " <> eraStr


-- There is likely something wrong with the approach of trying to coerce
-- this to a Conway era's PParams.
upgradeLedgerPParams :: forall crypto . ()
  => Crypto.Crypto crypto
  => Default.Default (Ledger.UpgradePParams Identity (Ledger.ConwayEra crypto))
  => Ledger.PParams (Ledger.BabbageEra crypto)
  -> Ledger.PParams (Ledger.ConwayEra crypto)
upgradeLedgerPParams ledgerParams =
  Ledger.upgradePParams (Default.def :: Ledger.UpgradePParams Identity (Ledger.ConwayEra crypto)) ledgerParams


-- There are missing fields here, never mind that nothing is really done
-- to set up any sorts of governance actions. Api.inEonForEra seemed to
-- run out of steam here.
mkTxBodyContent :: forall era . ()
  => Api.IsShelleyBasedEra era
  => Api.LedgerProtocolParameters era
  -> [Fund]
  -> (Api.TxInsCollateral era, [Fund])
  -> Api.TxFee era
  -> Api.TxMetadataInEra era
  -> [Api.TxOut Api.CtxTx era]
  -> Api.TxBodyContent Api.BuildTx era
mkTxBodyContent
     ledgerParameters
     inFunds
     (collateral, _collFunds)
     fee
     metadata
     outputs =
  Api.defaultTxBodyContent shelleyBasedEra
    & Api.setTxIns (map getTxIn inFunds)
    & Api.setTxInsCollateral collateral
    & Api.setTxOuts outputs
    & Api.setTxFee fee
    & Api.setTxValidityLowerBound Api.TxValidityNoLowerBound
    & Api.setTxValidityUpperBound (Api.defaultTxValidityUpperBound shelleyBasedEra)
    & Api.setTxMetadata metadata
    & Api.setTxProtocolParams (Api.BuildTxWith $ Just ledgerParameters)
  where
    shelleyBasedEra :: Api.ShelleyBasedEra era = Api.shelleyBasedEra
    getTxIn :: forall shelleyBasedEra . ()
      => Api.IsCardanoEra shelleyBasedEra
      => Fund
      -> (Api.TxIn, Api.BuildTxWith Api.BuildTx (Api.Witness Api.WitCtxTxIn shelleyBasedEra))
    getTxIn = FundQueue.getFundTxIn &&& Api.BuildTxWith . FundQueue.getFundWitness


-- This in itself is somewhat immune to the missing pieces, though it'll
-- end up being in a failure's callstack, as the txGenerator argument
-- comes from generateTx, which in turn invokes the functions missing the
-- transaction setup of fields and logic surrounding governance actions.
localSourceToStoreTransaction :: forall monad era split . ()
  => Monad monad
  => TxGenerator era
  -> FundSource monad
  -> ([Coin] -> split)
  -> UTxO.ToUTxOList era split
  -> FundToStoreList monad
  -> monad (Either TxGenError (Api.Tx era))
localSourceToStoreTransaction txGenerator fundSource inToOut mkTxOut fundToStore =
  fundSource >>= either (return . Left) go
 where
  go :: [Fund] -> monad (Either TxGenError (Api.Tx era))
  go inputFunds = do
    let
      -- 'getFundCoin' unwraps the 'TxOutValue' in a fund field
      -- so it's all just 'Lovelace' instead of a coproduct
      -- maintaining distinctions.
      outValues :: split = inToOut $ map FundQueue.getFundCoin inputFunds
      (outputs :: [Api.TxOut Api.CtxTx era], toFunds :: Api.TxId -> [Fund]) = mkTxOut outValues
    case txGenerator inputFunds outputs of
        Left (err :: TxGenError) -> return $ Left err
        Right (tx :: Api.Tx era, txId :: Api.TxId) -> do
          fundToStore $ toFunds txId
          return $ Right tx


-- This wrapper is largely to reorder arguments.
mkSignedTx' :: forall era . ()
  => Api.IsShelleyBasedEra era
  => Api.LedgerProtocolParameters era
  -> (Api.TxInsCollateral era, [Fund])
  -> Api.TxFee era
  -> Api.TxMetadataInEra era
  -> [Fund]
  -> [Api.TxOut Api.CtxTx era]
  -> Either Api.TxBodyError (Api.Tx era, Api.TxId)
mkSignedTx' ledgerParameters collateralFunds fee metadata inFunds outputs =
  mkSignedTx ledgerParameters inFunds collateralFunds fee metadata outputs

generateTx :: forall era . ()
  => Api.IsShelleyBasedEra era
  => TxEnvironment era
  -> Generator (Either TxGenError (Api.Tx era))
generateTx TxEnvironment{..}
  = localSourceToStoreTransaction
        generator
        consumeInputFunds
        computeOutputValues
        utxoList
        addNewOutputFunds
  where
    fee :: Coin
    Api.TxFeeExplicit _ fee = txEnvFee
    generator :: TxGenerator era
    generator =
        case Api.convertToLedgerProtocolParameters shelleyBasedEra txEnvProtocolParams of
          Right ledgerParameters ->
            -- While it could be golfed, it could end up being hard to read:
            -- (Arrow.left ApiError .) . mkSignedTx'' ledgerParameters
            \inFunds outputs -> Arrow.left ApiError $
                    mkSignedTx'' ledgerParameters inFunds outputs
          Left err -> \_ _ -> Left $ ApiError err
      where
        -- This is just useful for the comment attached to mkSignedTx''
        -- uncurry6 :: (t -> u -> v -> w -> x -> y -> z) -> (t, u, v, w, x, y) -> z
        -- uncurry6 f (t, u, v, w, x, y) = f t u v w x y
        mkSignedTx'' :: Api.LedgerProtocolParameters era
                     -> [Fund]
                     -> [Api.TxOut Api.CtxTx era]
                     -> Either Api.TxBodyError (Api.Tx era, Api.TxId)
        -- This might be a little long in the tooth.
        -- mkSignedTx'' ledgerParameters = (uncurry6 mkSignedTx' .) .
          -- (ledgerParameters, collateralFunds, txEnvFee, txEnvMetadata, ,)
        -- There's enough going on that's hard to keep track of that
        -- it's worth avoiding eta reducing this despite hlint's
        -- potential complaints.
        mkSignedTx'' ledgerParameters inFunds outputs =
            mkSignedTx' ledgerParameters
                        collateralFunds
                        txEnvFee
                        txEnvMetadata
                        inFunds
                        outputs
        shelleyBasedEra :: Api.ShelleyBasedEra era = Api.shelleyBasedEra
        -- collateralFunds are needed for Plutus transactions
        collateralFunds :: (Api.TxInsCollateral era, [Fund])
        collateralFunds = (Api.TxInsCollateralNone, [])

    -- Create a transaction that uses all the available funds.
    consumeInputFunds :: Generator (Either TxGenError [Fund])
    consumeInputFunds = do
      funds <- FundQueue.toList <$> get
      put FundQueue.emptyFundQueue
      return $ Right funds

    addNewOutputFunds :: [Fund] -> Generator ()
    addNewOutputFunds = put . List.foldl' FundQueue.insertFund FundQueue.emptyFundQueue

    computeOutputValues :: [Coin] -> [Coin]
    computeOutputValues = TxGen.inputsToOutputsWithFee fee numOfOutputs
      where numOfOutputs = 2

    computeUTxO :: UTxO.ToUTxO era
    computeUTxO = UTxO.mkUTxOVariant txEnvNetworkId signingKey

    utxoList :: UTxO.ToUTxOList era [Coin]
    utxoList = UTxO.makeToUTxOList $ repeat computeUTxO


generateTxM :: forall era . ()
  => Api.IsShelleyBasedEra era
  => TxEnvironment era
  -> Generator (Either TxGenError (Api.Tx era))
generateTxM txEnv
  = do
      inFunds <- get
      case generateTxPure txEnv inFunds of
        Right (tx, outFunds)  -> put outFunds >> pure (Right tx)
        Left err              -> pure (Left err)

generateTxPure :: forall era . ()
  => Api.IsShelleyBasedEra era
  => TxEnvironment era
  -> FundQueue
  -> Either TxGenError (Api.Tx era, FundQueue)
generateTxPure TxEnvironment{..} inQueue = do
  (tx, txId) <- generator inputs outputs
  pure (tx, FundQueue.fromList $ toFunds txId)
  where
    inputs = FundQueue.toList inQueue
    Api.TxFeeExplicit _ fee = txEnvFee
    generator :: TxGenerator era
    generator =
        case Api.convertToLedgerProtocolParameters shelleyBasedEra
                                                   txEnvProtocolParams of
          Right ledgerParameters ->
            \inFunds outs -> Arrow.left ApiError $
                mkSignedTx' ledgerParameters collateralFunds txEnvFee txEnvMetadata inFunds outs
          Left err -> \_ _ -> Left $ ApiError err

    shelleyBasedEra :: Api.ShelleyBasedEra era = Api.shelleyBasedEra
    -- collateralFunds are needed for Plutus transactions
    collateralFunds :: (Api.TxInsCollateral era, [Fund])
    collateralFunds = (Api.TxInsCollateralNone, [])

    outValues :: [Coin]
    outValues = computeOutputValues $ map FundQueue.getFundCoin inputs

    -- toFunds' type comes from this:
    -- type ToUTxOEra era [Coin] =  [Coin]
    --                           -> ([Api.TxOut Ctx era], TxId -> [Fund])
    toFunds :: Api.TxId -> [Fund]
    outputs :: [Api.TxOut Api.CtxTx era]
    (outputs, toFunds) = UTxO.makeToUTxOList (repeat computeUTxO) outValues

    computeOutputValues :: [Coin] -> [Coin]
    computeOutputValues = TxGen.inputsToOutputsWithFee fee numOfOutputs
      where numOfOutputs = 2

    computeUTxO :: UTxO.ToUTxO era
    computeUTxO = UTxO.mkUTxOVariant txEnvNetworkId signingKey
