{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Benchmarking.ScriptAPI
  ( PlutusBenchScript
  , psName
  , psScript
  , mkPlutusBenchScript
  , prepareScriptName
  ) where

import           Prelude as Haskell (String, dropWhile, head, last, map, reverse, (.), (==))
import           Data.Text (pack, split, unpack)
import           Cardano.Api (ScriptInAnyLang)

data PlutusBenchScript
  = PlutusBenchScript
    { psName   :: String
    , psScript :: ScriptInAnyLang
    }

mkPlutusBenchScript :: String -> ScriptInAnyLang -> PlutusBenchScript
mkPlutusBenchScript = PlutusBenchScript

prepareScriptName :: String -> String
prepareScriptName
  = head
  . dropWhile (=="hs")
  . map unpack
  . reverse
  . split (=='.')
  . last
  . split (=='/')
  . pack
