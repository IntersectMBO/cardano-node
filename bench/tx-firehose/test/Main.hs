{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Cardano.Api qualified as Api
import Cardano.Benchmarking.TxFirehose.Color (colorBytes, colorFromPublicKey, colorMetadataLabel)
import Cardano.Benchmarking.TxFirehose.Growth.State qualified as G
import Cardano.Benchmarking.TxFirehose.Tx
import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.Api.Tx.AuxData (hashTxAuxData)
import Cardano.Ledger.Coin (Coin (Coin), compactCoinOrError)
import Control.Monad (forM_)
import Data.Aeson qualified as JSON
import Data.ByteString qualified as BS
import Data.Foldable (toList)
import Data.Function ((&))
import Data.List (isInfixOf)
import Data.Map.Strict qualified as Map
import Data.Maybe.Strict (StrictMaybe (SJust, SNothing))
import Data.Proxy (Proxy (Proxy))
import Data.Set qualified as Set
import Data.Word (Word64)
import Lens.Micro ((.~), (^.))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

main :: IO ()
main =
  defaultMain $
    testGroup
      "tx-firehose"
      [ testCase "Conway growth checks realistic minimum ADA including change" $ do
          sk <- Api.generateSigningKey Api.AsPaymentKey
          let sbe = Api.ShelleyBasedEraConway
              network = Api.Testnet (Api.NetworkMagic 42)
              addr =
                Api.shelleyAddressInEra sbe $
                  Api.makeShelleyAddress
                    network
                    (Api.PaymentCredentialByKey (Api.verificationKeyHash (Api.getVerificationKey sk)))
                    Api.NoStakeAddress
              inputRef = case sk of
                Api.PaymentSigningKey key ->
                  Api.genesisUTxOPseudoTxIn network $
                    Api.verificationKeyHash (Api.getVerificationKey (Api.GenesisUTxOSigningKey key))
              input = Fund (Api.toShelleyTxIn inputRef) 1_000_000_000
              pp :: L.PParams (Api.ShelleyLedgerEra Api.ConwayEra)
              pp =
                L.emptyPParams
                  & L.ppTxFeePerByteL .~ L.CoinPerByte (compactCoinOrError (Coin 44))
                  & L.ppTxFeeFixedL .~ Coin 155_381
                  & L.ppMaxTxSizeL .~ 16_384
                  & L.ppCoinsPerUTxOByteL .~ L.CoinPerByte (compactCoinOrError (Coin 4310))
              color = colorFromPublicKey (Api.getVerificationKey sk)
          tx <- requireRight $ fitFanout sbe pp addr addr sk input 200 2_000_000 color
          length (btxOutputs tx) @?= 201
          assertLeft "minimum UTxO" $ fitFanout sbe pp addr addr sk input 200 1 color
          tinyChange <- requireRight $ buildFanout sbe addr addr sk input 1 999_799_999 (Coin 200_000) color
          assertLeft "minimum UTxO" $ validateTx sbe pp tinyChange
      , eraTests Api.ShelleyBasedEraShelley
      , eraTests Api.ShelleyBasedEraAllegra
      , eraTests Api.ShelleyBasedEraMary
      , eraTests Api.ShelleyBasedEraAlonzo
      , eraTests Api.ShelleyBasedEraBabbage
      , eraTests Api.ShelleyBasedEraConway
      , eraTests Api.ShelleyBasedEraDijkstra
      ]

eraTests :: forall era. Api.ShelleyBasedEra era -> TestTree
eraTests sbe =
  Api.shelleyBasedEraConstraints sbe $
    testGroup
      (show sbe)
      [ testCase "upstream payload padding preserves colour, zero bytes and label 1023" $ fixture $ \sk addr funds -> do
          forM_ [0, 1, 64, 65, 4096] $ \padding -> do
            tx <- requireRight $ buildTxPadded sbe addr sk funds 1 fee (color sk) padding
            Map.lookup colorMetadataLabel (metadata tx) @?= Just (Api.TxMetaBytes (colorBytes (color sk)))
            let payload = case Map.lookup 1023 (metadata tx) of
                  Nothing -> []
                  Just (Api.TxMetaList xs) -> [b | Api.TxMetaBytes b <- xs]
                  _ -> error "unexpected padding"
            BS.concat payload @?= BS.replicate (fromIntegral padding) 0
            assertBool "metadata chunks fit" (all ((<= 64) . BS.length) payload)
            Map.member 0 (metadata tx) @?= False
      , testCase "fanout fits 16KiB, conserves funds, calculates fees and replays exactly" $ fixture $ \sk addr funds -> do
          retainedSk <- Api.generateSigningKey Api.AsPaymentKey
          let retained =
                Api.shelleyAddressInEra sbe $
                  Api.makeShelleyAddress
                    (Api.Testnet (Api.NetworkMagic 42))
                    (Api.PaymentCredentialByKey (Api.verificationKeyHash (Api.getVerificationKey retainedSk)))
                    Api.NoStakeAddress
              params = pp & L.ppMaxTxSizeL .~ 16_384
              input = (head funds){fundValue = 1_000_000_000_000}
          tx <- requireRight $ fitFanout sbe params addr retained sk input 1000 2_000_000 (color sk)
          let outputs = btxOutputs tx
              n = length outputs - 1
              actualFee = case btxSigned tx of
                Api.ShelleyTx _ ledgerTx -> ledgerTx ^. L.bodyTxL . L.feeTxBodyL
          assertBool "reduced to fit" (n >= 200 && n < 1000)
          assertBool "signed size" (btxSerializedSize tx <= 16_384)
          map fundValue (init outputs) @?= replicate n 2_000_000
          sum (map fundValue outputs) + Api.unCoin actualFee @?= fundValue input
          rebuilt <- requireRight $ buildFanout sbe addr retained sk input n 2_000_000 actualFee (color sk)
          bytes rebuilt @?= bytes tx
          case btxSigned tx of
            Api.ShelleyTx _ ledgerTx -> do
              let expected a =
                    Api.toShelleyTxOutAny sbe $
                      Api.TxOut
                        a
                        (Api.lovelaceToTxOutValue sbe (Coin 2_000_000))
                        Api.TxOutDatumNone
                        Api.ReferenceScriptNone
                  addresses = map (^. L.addrTxOutL) $ toList (ledgerTx ^. L.bodyTxL . L.outputsTxBodyL)
              init addresses @?= replicate n (expected retained ^. L.addrTxOutL)
              last addresses @?= (expected addr ^. L.addrTxOutL)
          small <- requireRight $ fitFanout sbe params addr retained sk input 7 2_000_000 (color sk)
          length (btxOutputs small) @?= 8
          assertLeft "insufficient" $
            fitFanout
              sbe
              params
              addr
              retained
              sk
              (input{fundValue = 1})
              200
              2_000_000
              (color sk)
      , testCase "journal replay, partial finality, rollback and reservation accounting" $ fixture $ \sk addr funds -> do
          a <- requireRight $ autoFee sbe pp $ \f -> buildFanout sbe addr addr sk (head funds) 11 2_000_000 f (color sk)
          b <- requireRight $ autoFee sbe pp $ \f -> buildFanout sbe addr addr sk (last (btxOutputs a)) 7 2_000_000 f (color sk)
          let record tx =
                G.Batch
                  (length (btxOutputs tx) - 1)
                  (case btxSigned tx of Api.ShelleyTx _ t -> Api.unCoin (t ^. L.bodyTxL . L.feeTxBodyL))
                  (G.saveFund (last (btxOutputs tx)))
              lane = G.Lane (G.saveFund (head funds)) [record a, record b] True
              present f = Set.singleton (G.reference f)
          JSON.eitherDecode (JSON.encode lane) @?= Right lane
          let ident = G.Identity 3 "testnet" "era" "funding" "retained" "colour" 18 1 11 2_000_000
              initial = G.Journal 0 ident Nothing True 0 [lane{G.pending = []}]
              firstLane = lane{G.pending = [record a]}
              delta = G.LaneDelta 1 0 firstLane
          recovered <- requireRight $ G.applyDelta initial delta
          G.revision recovered @?= 1
          G.reserved (G.lanes recovered) @?= 11
          G.applyDelta recovered delta @?= Right recovered
          JSON.eitherDecode (JSON.encode recovered) @?= Right recovered
          assertLeft "sequence" $ G.applyDelta initial (G.LaneDelta 2 0 firstLane)
          assertLeft "sequence" $ G.applyDelta initial (G.LaneDelta 1 1 firstLane)
          assertLeft "transition" $ G.applyDelta initial (G.LaneDelta 1 0 lane)
          assertLeft "transition" $
            G.applyDelta
              initial
              (G.LaneDelta 1 0 firstLane{G.anchor = G.change (record a)})
          G.reserved [lane] @?= 18
          G.advance (present (G.anchor lane)) lane @?= Right (0, lane)
          (n, partial) <- requireRight $ G.advance (present (G.change (record a))) lane
          n @?= 11
          G.reserved [partial] @?= 7
          G.advance (present (G.anchor partial)) partial @?= Right (0, partial)
          (m, done) <- requireRight $ G.advance (present (G.change (record b))) partial
          m @?= 7
          G.pending done @?= []
          G.advance (present (G.anchor done)) done @?= Right (0, done)
          fst <$> G.advance (present (G.change (record b))) lane @?= Right 18
          -- A volatile rollback changes which recipes need replay, never stable accounting.
          G.observedPrefix (present (G.anchor lane)) lane @?= Right 0
          G.observedPrefix (present (G.change (record b))) lane @?= Right 2
          assertLeft "no known" $ G.advance Set.empty lane
          let volatileLane = lane{G.finalizedAnchor = False}
          G.advance Set.empty volatileLane @?= Right (0, volatileLane)
          (initialGrowth, anchored) <- requireRight $ G.advance (present (G.anchor lane)) volatileLane
          initialGrowth @?= 0
          G.finalizedAnchor anchored @?= True
          assertLeft "no known" $ G.advance Set.empty anchored
      , testCase "omitted padding and targets below natural size preserve bytes" $ fixture $ \sk addr funds -> do
          base <- requireRight $ buildTx sbe addr sk funds 1 fee (color sk)
          forM_ [1, fromIntegral (btxSerializedSize base)] $ \target -> do
            tx <- requireRight $ buildTxSized sbe addr sk funds 1 fee (color sk) target pp
            bytes tx @?= bytes base
            Map.keys (metadata tx) @?= [colorMetadataLabel]
      , testCase "signed sizes around chunk/header boundaries, colour and CBOR roundtrip" $ fixture $ \sk addr funds -> do
          base <- requireRight $ buildTx sbe addr sk funds 1 fee (color sk)
          -- Include the first metadata entry, 23/24-byte strings, 64-byte
          -- chunks, 23/24-element lists, and more than 256 chunks.
          let targets = map (btxSerializedSize base +) ([1 .. 140] ++ [1500 .. 1600] ++ [17_000])
          forM_ targets $ \target -> do
            tx <- requireRight $ buildTxSized sbe addr sk funds 1 fee (color sk) (fromIntegral target) pp
            btxSerializedSize tx @?= BS.length (bytes tx)
            assertBool ("target " ++ show target) $
              btxSerializedSize tx >= target && btxSerializedSize tx <= target + 4
            Map.lookup colorMetadataLabel (metadata tx) @?= Just (Api.TxMetaBytes (colorBytes (color sk)))
            let payload = chunks tx
            assertBool "nonempty chunks <= 64 bytes" $
              not (null payload) && all (\b -> not (BS.null b) && BS.length b <= 64) payload
            Set.size (Set.fromList payload) @?= length payload
            decoded <-
              requireRight $
                Api.deserialiseFromCBOR (Api.AsTx (Api.proxyToAsType (Proxy :: Proxy era))) (bytes tx)
            Api.serialiseToCBOR decoded @?= bytes tx
            case btxSigned tx of
              Api.ShelleyTx _ ledgerTx -> do
                ledgerTx ^. L.bodyTxL . L.auxDataHashTxBodyL @?= (hashTxAuxData <$> (ledgerTx ^. L.auxDataTxL))
                Set.size (ledgerTx ^. L.witsTxL . L.addrTxWitsL) @?= 1
            -- Removing the final payload byte must fall below the target;
            -- otherwise we inserted more padding than necessary.
            let shorter = init payload ++ [BS.init (last payload) | BS.length (last payload) > 1]
            previous <-
              requireRight $
                buildTxWithMetadata sbe addr sk funds 1 fee $
                  metadataWithPadding (color sk) (map Api.TxMetaBytes shorter)
            assertBool "minimal padding" (btxSerializedSize previous < target)
      , testCase "padding is reproducible and includes every input reference" $ fixture $ \sk addr funds -> do
          let f0 = head funds
              sibling ix =
                f0
                  { fundTxIn = case Api.fromShelleyTxIn (fundTxIn f0) of
                      Api.TxIn txid _ -> Api.toShelleyTxIn (Api.TxIn txid (Api.TxIx ix))
                  }
              build fs = requireRight $ buildTxSized sbe addr sk fs 1 fee (color sk) 4096 pp
          a <- build [f0]
          again <- build [f0]
          b <- build [sibling 1]
          c <- build [f0, sibling 1]
          d <- build [f0, sibling 2]
          bytes a @?= bytes again
          assertBool "sibling output indices differ" (chunks a /= chunks b)
          assertBool "later inputs differ" (chunks c /= chunks d)
      , testCase "fan-out and consolidation retain outputs and funds" $ fixture $ \sk addr funds -> do
          fanout <- requireRight $ buildTxSized sbe addr sk funds 10 fee (color sk) 4096 pp
          length (btxOutputs fanout) @?= 10
          sum (map fundValue (btxOutputs fanout)) @?= sum (map fundValue funds) - 2_000_000
          consolidated <- requireRight $ buildTxSized sbe addr sk (btxOutputs fanout) 1 fee (color sk) 4096 pp
          btxInputs consolidated @?= 10
          length (btxOutputs consolidated) @?= 1
          assertBool "size re-evaluated for new shape" (btxSerializedSize consolidated >= 4096)
      , testCase "fixed fee checked against ledger minimum, including equality" $ fixture $ \sk addr funds -> do
          tx <- requireRight $ buildTxSized sbe addr sk funds 1 fee (color sk) 4096 pp
          let required = case btxSigned tx of
                Api.ShelleyTx _ ledgerTx -> L.getMinFeeTx pp ledgerTx 0
              Coin amount = required
          _ <- requireRight $ buildTxSized sbe addr sk funds 1 required (color sk) 4096 pp
          assertLeft "--fee" $ buildTxSized sbe addr sk funds 1 (Coin (amount - 1)) (color sk) 4096 pp
      , testCase "invalid and oversized targets fail before allocation" $ fixture $ \sk addr funds -> do
          forM_ [0, 65_537, 10 ^ (100 :: Int)] $ \target ->
            assertLeft "--tx-size" $ buildTxSized sbe addr sk funds 1 fee (color sk) target pp
          let tiny = pp & L.ppMaxTxSizeL .~ 100
          assertLeft "maxTxSize" $ buildTxSized sbe addr sk funds 1 fee (color sk) 100 tiny
          base <- requireRight $ buildTx sbe addr sk funds 1 fee (color sk)
          let limit = fromIntegral (btxSerializedSize base + 1)
          assertLeft "maxTxSize" $
            buildTxSized sbe addr sk funds 1 fee (color sk) (fromIntegral limit) (pp & L.ppMaxTxSizeL .~ limit)
      ]
 where
  fee = Coin 2_000_000
  color = colorFromPublicKey . Api.getVerificationKey
  pp :: L.PParams (Api.ShelleyLedgerEra era)
  pp =
    Api.shelleyBasedEraConstraints sbe $
      L.emptyPParams
        & L.ppTxFeePerByteL .~ L.CoinPerByte (compactCoinOrError (Coin 44))
        & L.ppTxFeeFixedL .~ Coin 155_381
        & L.ppMaxTxSizeL .~ 65_536
  fixture :: (Api.SigningKey Api.PaymentKey -> Api.AddressInEra era -> [Fund] -> IO ()) -> IO ()
  fixture action = do
    sk <- Api.generateSigningKey Api.AsPaymentKey
    let network = Api.Testnet (Api.NetworkMagic 42)
        addr =
          Api.shelleyAddressInEra sbe $
            Api.makeShelleyAddress
              network
              (Api.PaymentCredentialByKey (Api.verificationKeyHash (Api.getVerificationKey sk)))
              Api.NoStakeAddress
        input = case sk of
          Api.PaymentSigningKey key ->
            Api.genesisUTxOPseudoTxIn network $
              Api.verificationKeyHash (Api.getVerificationKey (Api.GenesisUTxOSigningKey key))
    action sk addr [Fund (Api.toShelleyTxIn input) 1_000_000_000]

bytes :: Api.IsShelleyBasedEra era => BuiltTx era -> BS.ByteString
bytes = Api.serialiseToCBOR . btxSigned

metadata :: Api.ShelleyBasedEraConstraints era => BuiltTx era -> Map.Map Word64 Api.TxMetadataValue
metadata tx = case btxSigned tx of
  Api.ShelleyTx _ ledgerTx -> case ledgerTx ^. L.auxDataTxL of
    SNothing -> Map.empty
    SJust aux -> Api.fromShelleyMetadata (aux ^. L.metadataTxAuxDataL)

chunks :: Api.ShelleyBasedEraConstraints era => BuiltTx era -> [BS.ByteString]
chunks tx = case Map.lookup 0 (metadata tx) of
  Just (Api.TxMetaList xs) -> [bs | Api.TxMetaBytes bs <- xs]
  _ -> []

requireRight :: Show e => Either e a -> IO a
requireRight = either (\err -> assertFailure (show err) >> fail "unreachable") pure

assertLeft :: String -> Either String a -> IO ()
assertLeft expected result = case result of
  Left err -> assertBool err (expected `isInfixOf` err)
  Right _ -> assertFailure ("expected failure containing " ++ expected)
