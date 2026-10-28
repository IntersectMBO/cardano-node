{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Cardano.Api qualified as Api
import Cardano.Benchmarking.TxFirehose.Color (colorBytes, colorFromPublicKey, colorMetadataLabel)
import Cardano.Benchmarking.TxFirehose.Tx
import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.Api.Tx.AuxData (hashTxAuxData)
import Cardano.Ledger.Coin (Coin (Coin))
import Control.Monad (forM_)
import Data.ByteString qualified as BS
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
      [ eraTests Api.ShelleyBasedEraShelley
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
      [ testCase "omitted padding and targets below natural size preserve bytes" $ fixture $ \sk addr funds -> do
          base <- requireRight $ buildTx sbe addr sk funds 1 fee (color sk)
          forM_ [1, fromIntegral (btxSerializedSize base)] $ \target -> do
            tx <- requireRight $ buildTxSized sbe addr sk funds 1 fee (color sk) target pp
            bytes tx @?= bytes base
            Map.keys (metadata tx) @?= [colorMetadataLabel]
      , testCase "signed sizes around chunk/header boundaries, colour and CBOR roundtrip" $ fixture $ \sk addr funds -> do
          base <- requireRight $ buildTx sbe addr sk funds 1 fee (color sk)
          -- Include the first metadata entry, 23/24-byte strings, 64-byte
          -- chunks, 23/24-element lists, and more than 256 chunks.
          let targets = map ((btxSerializedSize base) +) ([1 .. 140] ++ [1500 .. 1600] ++ [17000])
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
          forM_ [0, 65537, 10 ^ (100 :: Int)] $ \target ->
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
      L.emptyPParams & L.ppMinFeeAL .~ Coin 44 & L.ppMinFeeBL .~ Coin 155381 & L.ppMaxTxSizeL .~ 65536
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
