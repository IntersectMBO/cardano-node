module Cardano.JSON
  ( module Cardano.JSON
  , Value(..), Object)
where

import Prelude                  (error)
import Cardano.Prelude          hiding (head)

import Data.Aeson               (Value (..))
import Data.Aeson.Types         (Key, Object)
import Data.Aeson.KeyMap        qualified as KM
import Data.Aeson.KeyMap        (fromMapText)
import Data.Map.Strict          qualified as M


alterSubObject  :: (Object -> Maybe Object) -> Key -> Object -> Maybe Object
alterSubObject f k =
  KM.alterF (\case
              Just (Object o) -> Just . Object <$> f o
              Just x -> error $ mconcat [ "tryAlterObject: non-object at key "
                                        , show k, ": ", show x ]
              Nothing -> Nothing)
            k

mapSubObject  :: (Object -> Object) -> Key -> Object -> Object
mapSubObject f k =
  runIdentity .
  KM.alterF (\case
              Just (Object o) -> Identity . Just . Object $ f o
              Just x  -> error $ mconcat [ "tryAlterObject: non-object at key "
                                         , show k, ": ", show x ]
              Nothing -> error $ mconcat [ "tryAlterObject: missing key ", show k ])
            k

overlayJSON :: [(Text, Value)] -> Object -> Object
overlayJSON = (<>) . fromMapText . M.fromList
