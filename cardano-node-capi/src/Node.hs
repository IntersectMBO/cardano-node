module Node where

import qualified Cardano.Configuration as CC
import           Cardano.Node.Run (runNode)

import           Options.Applicative

import           Foreign.C (CString, peekCString)
import           Foreign.Marshal.Array (peekArray)
import           Foreign.Ptr (Ptr)

-- | @crunNode@ is an exported C entry point to start a node.
-- We parse the same arguments as the node CLI, but allow to
-- pass the arguments as @char *argv[]@ from C.
foreign export ccall "runNode" crunNode :: Int -> Ptr CString -> IO ()
crunNode :: Int -> Ptr CString -> IO ()
crunNode argc argv = peekArray argc argv >>= mapM peekCString >>= \args ->
    case execParserPure pref opts args of
        Success cli -> runNode cli
        Failure f   -> print f
        CompletionInvoked _ -> putStrLn "Completion Invoked?"
  where
    pref = prefs showHelpOnEmpty
    opts = info (nodeRunParser <**> helper)
        ( fullDesc <> progDesc "Start node of the Cardano blockchain." )
    nodeRunParser =
      subparser $
        command "run" (info (CC.parseCliArgs <**> helper) (progDesc "Run the node."))
