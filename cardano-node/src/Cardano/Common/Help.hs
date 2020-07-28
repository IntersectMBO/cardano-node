module Cardano.Common.Help
  ( parserHelpHeader
  , parserHelpOptions
  , renderHelpDoc
  -- * Re-exports
  , OptI.Doc
  , (<$$>)
  )
where

import           Prelude

import           Data.Maybe
  ( fromMaybe
  )
import qualified Options.Applicative as Opt
import           Options.Applicative.Help
  ( (<$$>)
  )
import qualified Options.Applicative.Help as OptI

-- | Produce just the brief help header for a given CLI option parser,
--   without the options.
parserHelpHeader :: String -> Opt.Parser a -> OptI.Doc
parserHelpHeader execName = flip (OptI.parserUsage (Opt.prefs mempty)) execName

-- | Produce just the options help for a given CLI option parser,
--   without the header.
parserHelpOptions :: Opt.Parser a -> OptI.Doc
parserHelpOptions = fromMaybe mempty . OptI.unChunk . OptI.fullDesc (Opt.prefs mempty)

-- | Render the help pretty document.
renderHelpDoc :: Int -> OptI.Doc -> String
renderHelpDoc cols =
  (`OptI.displayS` "") . OptI.renderPretty 1.0 cols
