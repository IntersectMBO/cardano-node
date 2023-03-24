{-# LANGUAGE BangPatterns #-}

import           Cardano.Api
import           Cardano.Binary (serialize')
import           Cardano.Ledger.Alonzo.TxInfo (PlutusDebug)
import           Control.Monad (when)
import           Control.Monad.Trans.Except (runExceptT)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Options.Applicative as Opt
import           Options.Applicative (Parser, (<**>))

data Args = Args
  { conf        :: String
  , socket      :: String
  , targetSlots :: [SlotNo]
  }

parseSlot :: Parser SlotNo
parseSlot = SlotNo <$> Opt.option Opt.auto
               ( Opt.long "slot"
              <> Opt.metavar "NATURAL"
              <> Opt.help "target slot numbers")

parser :: Parser Args
parser = Args
  <$> Opt.strOption
      ( Opt.long "conf"
     <> Opt.short 'c'
     <> Opt.metavar "FILEPATH"
     <> Opt.help "configuration file" )
  <*> Opt.strOption
      ( Opt.long "socket"
     <> Opt.short 's'
     <> Opt.metavar "FILEPATH"
     <> Opt.help "socket" )
  <*> Opt.many parseSlot

displayPlutusDebug :: PlutusDebug -> String
displayPlutusDebug = BS.unpack . B64.encode . serialize'

displayPlutusDebugs :: NE.NonEmpty PlutusDebug -> [String]
displayPlutusDebugs pdbs = displayPlutusDebug <$> NE.toList pdbs

displayPlutusEvents :: LedgerEvent -> [String]
displayPlutusEvents (SuccessfulPlutusScript pdbs) = "SUCCESS" : displayPlutusDebugs pdbs
displayPlutusEvents (FailedPlutusScript pdbs) = "FAILURE" : displayPlutusDebugs pdbs
displayPlutusEvents _ = []

printPlutusEvents :: [LedgerEvent] -> IO ()
printPlutusEvents = mapM_ $ mapM_ putStrLn . displayPlutusEvents

parserInfo :: Opt.ParserInfo Args
parserInfo = Opt.info (parser <**> Opt.helper) (Opt.fullDesc <> Opt.progDesc "Plutus Events")

slotFromBlockInMode :: BlockInMode b -> SlotNo
slotFromBlockInMode (BlockInMode (Block (BlockHeader slot _bHeaderHash _blockNo) _txs) _era) = slot

f :: [SlotNo] -> a -> b -> [LedgerEvent] -> BlockInMode c -> d -> IO ()
f ts _ _ events bim _ = when (slotFromBlockInMode bim `elem` ts) (printPlutusEvents events)

main :: IO ()
main = do
  args <- Opt.execParser parserInfo
  let r = either (error . T.unpack . renderFoldBlocksError) id
  !_ <- fmap r . runExceptT $ foldBlocks
         (conf args)
         (socket args)
         FullValidation
         ()
         (f $ targetSlots args)
  pure ()
