{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs        #-}

import           Cardano.Api
import           Cardano.Api.Shelley
import           Cardano.Ledger.Alonzo.TxInfo (PlutusDebug(..), PlutusDebugLang(..))
import           Cardano.Ledger.BaseTypes (ProtVer(..))
import           Cardano.Ledger.Binary (serialize')
import           Cardano.Ledger.Language (SLanguage(..))
import           Control.Monad (when)
import           Control.Monad.Trans.Except (runExceptT)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Options.Applicative as Opt
import           Options.Applicative (Parser, (<**>))
import           Data.String (IsString (..))

data Args = Args
  { conf         :: String
  , socket       :: String
  , targetSlots  :: [SlotNo]
  , targetScript :: [ScriptHash]
  }

parseSlot :: Parser SlotNo
parseSlot =
  SlotNo <$> Opt.option Opt.auto
     ( Opt.long "slot"
    <> Opt.metavar "NATURAL"
    <> Opt.help
         (concat
            [ "Restrict to the given slot numbers. "
            , "If not used, no slot restrictions are imposed. "
            , "This field can be supplied multiple times."
            ]
     ))

parseScriptHash :: Parser ScriptHash
parseScriptHash =
  fromString <$> Opt.strOption
      ( Opt.long "hash"
     <> Opt.metavar "SCRIPTHASH"
     <> Opt.help
          (concat
            [ "Restrict to the given script hashes. "
            , "If not used, no script hash restrictions are imposed. "
            , "This field can be supplied multiple times."
            ]
      ))

parser :: Parser Args
parser = Args
  <$> Opt.strOption
      ( Opt.long "conf"
     <> Opt.short 'c'
     <> Opt.metavar "FILEPATH"
     <> Opt.help "node configuration file" )
  <*> Opt.strOption
      ( Opt.long "socket"
     <> Opt.short 's'
     <> Opt.metavar "FILEPATH"
     <> Opt.help "node socket" )
  <*> Opt.many parseSlot
  <*> Opt.many parseScriptHash

scriptHashFromPlutusDebug :: PlutusDebug -> ScriptHash
scriptHashFromPlutusDebug (PlutusDebug (PlutusDebugLang SPlutusV1 _ _ s _ _)) =
  hashScript $ PlutusScript PlutusScriptV1 (PlutusScriptSerialised s)
scriptHashFromPlutusDebug (PlutusDebug (PlutusDebugLang SPlutusV2 _ _ s _ _)) =
  hashScript $ PlutusScript PlutusScriptV2 (PlutusScriptSerialised s)

isTarget :: Eq a => [a] -> a -> Bool
isTarget xs a =
  case xs of
    [] -> True -- We impose no restrictions if empty
    _  -> a `elem` xs

isTargetedSlot :: [SlotNo] -> BlockInMode b -> Bool
isTargetedSlot slots bim = isTarget slots (slotFromBlockInMode bim)

isTargetedScript :: [ScriptHash] -> PlutusDebug -> Bool
isTargetedScript ts pdb = isTarget ts (scriptHashFromPlutusDebug pdb)

data PlutusResult = Success | Failure
  deriving (Show)

displayPlutusDebug :: PlutusResult -> PlutusDebug -> String
displayPlutusDebug pr pdb@(PlutusDebug pdl) =
  concat
    [ "Execution: ", show pr, "\n"
    , "Script Hash: ", hexScriptHash, "\n"
    , "Plutus Debug: ", encodedPDB
    ]
  where
    hexScriptHash = show (scriptHashFromPlutusDebug pdb)
    encodedPDB = BS.unpack . B64.encode $ serialize' (pvMajor . pdProtVer $ pdl) pdb

displayPlutusDebugs :: PlutusResult -> [ScriptHash] -> NE.NonEmpty PlutusDebug -> [String]
displayPlutusDebugs pr targets pdbs =
  [displayPlutusDebug pr pdb | pdb <- NE.toList pdbs, isTargetedScript targets pdb]

displayPlutusEvents :: [ScriptHash] -> LedgerEvent -> [String]
displayPlutusEvents targets (SuccessfulPlutusScript pdbs) = displayPlutusDebugs Success targets pdbs
displayPlutusEvents targets (FailedPlutusScript pdbs) = displayPlutusDebugs Failure targets pdbs
displayPlutusEvents _ _ = []

printPlutusEvents :: [ScriptHash] -> [LedgerEvent] -> IO ()
printPlutusEvents targets = mapM_ $ mapM_ putStrLn . displayPlutusEvents targets

description :: String
description = concat
  [ "This is a tool which provides debugging information for any Plutus script "
  , "included in the chain history. "
  , "The debugging information includes the script, the execution units, the script context, "
  , "and the cost model. "
  , "This tool requires a running node. "
  , "The cardano-ledger repository provides a function 'debugPlutus' in the"
  , "cardano-ledger-alonzo package which will replay the script and provide the Plutus logs. "
  , "The debugging information is first CBOR encoded and then base 64 encoded, "
  , "but note that 'debugPlutus' expects the input to be base 64 encoded."
  ]

parserInfo :: Opt.ParserInfo Args
parserInfo = Opt.info (parser <**> Opt.helper) (Opt.fullDesc <> Opt.progDesc description)

slotFromBlockInMode :: BlockInMode b -> SlotNo
slotFromBlockInMode (BlockInMode (Block (BlockHeader slot _bHeaderHash _blockNo) _txs) _era) = slot

printWhenTargeted :: Args -> a -> b -> [LedgerEvent] -> BlockInMode c -> d -> IO ()
printWhenTargeted args _ _ events bim _ =
  when
    (isTargetedSlot (targetSlots args) bim)
    (printPlutusEvents (targetScript args) events)

main :: IO ()
main = do
  args <- Opt.execParser parserInfo
  let r = either (error . T.unpack . renderFoldBlocksError) id
  !_ <- fmap r . runExceptT $ foldBlocks
         (File (conf args))
         (File (socket args))
         FullValidation
         ()
         (printWhenTargeted args)
  pure ()
