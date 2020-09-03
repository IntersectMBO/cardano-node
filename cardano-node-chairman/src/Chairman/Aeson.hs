module Chairman.Aeson
  ( rewriteObject
  ) where

import           Data.Aeson
import           Data.HashMap.Lazy
import           Data.Text

rewriteObject :: (HashMap Text Value -> HashMap Text Value) -> Value -> Value
rewriteObject f (Object hm) = Object (f hm)
rewriteObject _ v = v
