{-# LANGUAGE OverloadedStrings #-}

module Test.Cli.TxBuildRaw
  ( tests
  ) where

import           Cardano.Prelude
import           Hedgehog (Property)
import           Test.OptParse

import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import Prelude (String)
import qualified Data.Text as T
import Cardano.Api
import Control.Monad (fail)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Aeson as A

type KeyFilePair = (FilePath,FilePath)

-- tests transaction build for different outputs and mint option.
-- Note that the transaction need not be balanced, we are just testing the building part
tests :: IO Bool
tests =
  H.checkSequential
    $ H.Group "Cli.TxBuildRaw"
        [    ("prop_txBuildRaw_ZeroLovelaceOutput_legacy", testTxBuildSimple [
                  "--tx-in", "91999ea21177b33ebe6b8690724a0c026d410a11ad7521caa350abdafa5394c3#0"
                , "--tx-out", "addr1v9wmu83pzajplrtpsq6tsqdgwr98x888trpmah2u0ezznsge7del3 0 lovelace"])
          , ("prop_txBuildRaw_ZeroLovelaceOutput", testTxBuildSimple [
                  "--tx-in", "91999ea21177b33ebe6b8690724a0c026d410a11ad7521caa350abdafa5394c3#0"
                , "--tx-out", "addr1v9wmu83pzajplrtpsq6tsqdgwr98x888trpmah2u0ezznsge7del3+0"])
          , ("prop_txBuildRaw_MultpleOutputs", testTxBuildSimple [
                  "--tx-in", "91999ea21177b33ebe6b8690724a0c026d410a11ad7521caa350abdafa5394c3#0"
                , "--tx-out", "addr1v9wmu83pzajplrtpsq6tsqdgwr98x888trpmah2u0ezznsge7del3+100000000"
                , "--tx-out", "addr1qxlfgax2pks5pxfm82k9z59v7s8vmx30am57sgh9gyucf50ee858y3kj7qmn3pvfdtfgqjmj99nnypx2eysgx3wpafdss8e9zn+ 2000000"])
          , ("prop_txBuildRaw_MultiAssetOutput", testTxBuildSimple [
                  "--tx-in", "91999ea21177b33ebe6b8690724a0c026d410a11ad7521caa350abdafa5394c3#0"
                , "--tx-out", "addr1qxlfgax2pks5pxfm82k9z59v7s8vmx30am57sgh9gyucf50ee858y3kj7qmn3pvfdtfgqjmj99nnypx2eysgx3wpafdss8e9zn"
                    <> "+ 2000000"
                    <> "+ 1 bdcc2c63bb7c0f2dcdefd9da8b40c7a894d53fc9bde15c5423caf1d8.abcd"
                    <> " + 3 bdcc2c63bb7c0f2dcdefd9da8b40c7a894d53fc9bde15c5423caf1d8.ef0123"])
          , ("prop_txBuildRaw_Mint", testTxBuildWithMint 10 [
                  "--tx-in", "91999ea21177b33ebe6b8690724a0c026d410a11ad7521caa350abdafa5394c3#0"
                ,  "--tx-out", "addr1v9wmu83pzajplrtpsq6tsqdgwr98x888trpmah2u0ezznsge7del3+100000000"
                , "--tx-out", "addr1qxlfgax2pks5pxfm82k9z59v7s8vmx30am57sgh9gyucf50ee858y3kj7qmn3pvfdtfgqjmj99nnypx2eysgx3wpafdss8e9zn+2000000"])
          , ("prop_txBuildRaw_Burn", testTxBuildWithMint (-10) [
                  "--tx-in", "91999ea21177b33ebe6b8690724a0c026d410a11ad7521caa350abdafa5394c3#0"
                ,  "--tx-out", "addr1v9wmu83pzajplrtpsq6tsqdgwr98x888trpmah2u0ezznsge7del3+100000000"])
        ]

testTxBuildSimple :: [String] -> Property
testTxBuildSimple cmds =  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  keys <- genKeys tempDir
  testTxBuildRawPropertyT keys cmds tempDir

testTxBuildWithMint :: Integer ->   [String] -> Property
testTxBuildWithMint quantity cmds =  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    let mintingScriptFile = tempDir<> "minting-script-file"
    kPair<- genKeys tempDir
    policyIdStr <- writeRequireSigScript  kPair mintingScriptFile <&> scriptHashString
    let mintCmds = [
              "--mint",show quantity <> " "<> policyIdStr <> ".012345"
            , "--mint-script-file" ,mintingScriptFile
          ]
    testTxBuildRawPropertyT kPair (mintCmds ++ cmds) tempDir


writeRequireSigScript :: KeyFilePair -> FilePath -> H.PropertyT IO (Script SimpleScriptV1)
writeRequireSigScript (_,vkeyFile) scriptFilePath=  do
    vkeyString<- H.lbsReadFile vkeyFile
    testEnvelope <-case A.decode vkeyString of
      Nothing -> fail "Unexpected: error parsing verificationKeyFile"
      Just v -> pure v
    vkey <- case deserialiseFromTextEnvelope  ( AsVerificationKey AsPaymentKey)  testEnvelope of
            Left _ -> fail "Unexpected: error decoding verificationKeyFile"
            Right k -> pure k
    let script = RequireSignature (verificationKeyHash vkey)
    H.lbsWriteFile  scriptFilePath ( BS.fromStrict $ serialiseToJSON script)
    pure (SimpleScript SimpleScriptV1 script)


scriptHashString :: Script lang -> String
scriptHashString script = let scriptHash = hashScript   script
  in
    T.unpack $ serialiseToRawBytesHexText scriptHash

genKeys :: FilePath -> H.PropertyT IO (FilePath, FilePath)
genKeys tempDir=  do
  paymentSignKey <- noteTempFile tempDir "payment-signing-key-file"
  paymentVerKey <- noteTempFile tempDir "payment-verification-key-file"
    -- Generate payment signing key to sign transaction
  void $ execCardanoCLI
    [ "address","key-gen"
    , "--verification-key-file", paymentVerKey
    , "--signing-key-file", paymentSignKey
    ]
  H.assertFilesExist [paymentVerKey, paymentSignKey]
  pure (paymentSignKey,paymentVerKey)



testTxBuildRawPropertyT ::KeyFilePair -> [String]  ->   FilePath ->  H.PropertyT IO ()
testTxBuildRawPropertyT (paymentSignKey,_) cmds tempDir = do
  transactionBodyFile <- noteTempFile tempDir "transaction-body"
  transactionFile <- noteTempFile tempDir "transaction-file"

  -- Create transaction body
  void $ execCardanoCLI $
      [ "transaction", "build-raw"]
      ++ cmds
      ++ [
        "--fee", "1000000"
        , "--invalid-hereafter", "500000"
        , "--out-file", transactionBodyFile
      ]

  H.assertFilesExist [transactionBodyFile]
  -- Sign transaction
  void $ execCardanoCLI
    [ "transaction", "sign"
    , "--tx-body-file", transactionBodyFile
    , "--signing-key-file", paymentSignKey
    , "--mainnet"
    , "--out-file", transactionFile
    ]