module Node where

import           Data.Aeson (eitherDecodeStrict)
import           Cardano.Node.Run (runNode)

import           Foreign.Ptr (Ptr)
import           Foreign.C (CString, peekCString)
import           Foreign.Marshal.Array (peekArray)
import           Data.ByteString.Char8 (pack)

import           Options.Applicative
import           Cardano.Node.Parsers (nodeCLIParser, parserHelpHeader, parserHelpOptions,
                   renderHelpDoc)

-- | @crunNode@ is an exported C entry point to start a node.
-- We parese the same arguments as the node CLI, but allow to
-- pass the arguments as @char *argv[]@ from C.
foreign export ccall "runNode" crunNode :: Int -> Ptr CString -> IO ()
crunNode :: Int -> Ptr CString -> IO ()
crunNode argc argv = peekArray argc argv >>= mapM peekCString >>= \args ->
    case execParserPure pref opts args of
        Success pnc -> runNode pnc
        Failure f   -> print f
        CompletionInvoked _ -> putStrLn "Completion Invoked?"
  where
    pref = prefs showHelpOnEmpty
    opts = info nodeCLIParser
        ( fullDesc <> progDesc "Start node of the Cardano blockchain." )
