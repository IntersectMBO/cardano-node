{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.CLI.Faucet.Web where

import Servant
import Cardano.Prelude
import Cardano.Api (CardanoEra, IsShelleyBasedEra, ShelleyBasedEra, TxInMode(TxInMode), getTxId, TxId)
import Data.ByteString.Lazy     qualified as LBS
import Cardano.CLI.Faucet.Types
import Data.Text qualified as T
import Cardano.CLI.Faucet.Misc
import Data.Aeson (encode)
import Cardano.CLI.Faucet.Utils
import Cardano.CLI.Faucet.TxUtils
import Cardano.CLI.Types
import Control.Concurrent.STM (writeTQueue)
import Data.String
import Cardano.CLI.Run.Friendly (friendlyTxBS)

-- https://faucet.cardano-testnet.iohkdev.io/send-money/addr_test1vr3g684kyarnug89c7p7gqxr5y8t45g7q4ge4u8hghndvugn6yn5s?apiKey=&g-recaptcha-response=03AGdBq24qppnXuY6fIcCG2Hrpqxfp0V9Xd3oDqElSikr38sAuPMmpO4dKke9O0NzhtFnv_-cXVSs8h4loNDBDeM3rIb5UDHmoCsIylCHXmOovfDIOWM7417-9nW_8XegF7murR2CpVGDp8js7L33ygKqbPUus8AQncJ26AikCDiDNOe7_u6pHb20pR_a8a2cjfcRu6Ptrq8uTWxk2QiinvSctAZbnyTRscupJNDVvoJ1l52LNXOFNTFowRuyaRu1K9mLAJvbwy5n1il_05UGWRNvK3raCUA1DKhf0l9yOCfEvoNJNp10rTG5JFWeYaIiI3-ismQITIsR3u4akYy1PPjmNyF12vfcjlgbvXdGOcodyiZvKulnp2XNSQVIu-OHiwERumU5IISD9VRzY804Z1tKkRB7_PxpUvE7SOAKdOqmkvZLMn8ob1Fz8I562qiV8oezkVkSqTfqQbK2Vsqn3dYDd-IY0pjUhnw
-- http[s]://$FQDN:$PORT/send-money/$ADDRESS

type SendMoney = "send-money" :> Capture "destination_address" Text :> Post '[OctetStream] LBS.ByteString
type RootDir = SendMoney

userAPI :: Proxy RootDir
userAPI = Proxy

server :: IsShelleyBasedEra era =>
  CardanoEra era
  -> ShelleyBasedEra era
  -> FaucetState era
  -> Server RootDir
server era sbe faucetState = handleSendMoney era sbe faucetState

handleSendMoney :: IsShelleyBasedEra era =>
  CardanoEra era
  -> ShelleyBasedEra era
  -> FaucetState era
  -> Text
  -> Servant.Handler LBS.ByteString
handleSendMoney era sbe FaucetState{network,utxoTMVar,skey,queue} addr = do
  eResult <- liftIO $ runExceptT $ do
    addressAny <- parseAddress addr
    txinout@(txin,_) <- findUtxoOfSize utxoTMVar 1000170000
    putStr @Text "selected the following txin: "
    print txin
    eraInMode <- convertEra era
    -- instead of having to specify an output that is exactly equal to input-fees
    -- i specify no outputs, and set the change addr to the end-user
    unsignedTx <- txBuild sbe defaultCModeParams network txinout [] (TxOutChangeAddress addressAny)
    let
      txid :: TxId
      txid = getTxId unsignedTx
    putStrLn @Text $ "new txid: " <> (show txid)
    signedTx <- withExceptT FaucetErrorTodo $ txSign network unsignedTx [skey]
    let
      prettyTx = friendlyTxBS era signedTx
    liftIO $ atomically $ writeTQueue queue (TxInMode signedTx eraInMode, prettyTx)
    return $ SendMoneyReply txid txin
  case eResult of
    Right msg -> pure $ encode msg
    Left err -> pure . fromString . T.unpack . renderFaucetError $ err
