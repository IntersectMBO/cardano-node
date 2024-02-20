module Node where

import           Cardano.Node.Parsers (nodeCLIParser, parserHelpHeader, parserHelpOptions,
                   renderHelpDoc)
import           Cardano.Node.Run (runNode)

import           Data.Aeson (eitherDecodeStrict)
import           Data.ByteString.Char8 (pack)
import           Options.Applicative

import           Foreign.C (CString, peekCString)
import           Foreign.Marshal.Array (peekArray)
import           Foreign.Ptr (Ptr)

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
