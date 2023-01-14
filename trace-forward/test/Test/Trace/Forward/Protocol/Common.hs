{-# LANGUAGE FlexibleInstances #-}

module Test.Trace.Forward.Protocol.Common
  ( splits2
  , splits3
  ) where

import qualified Data.ByteString.Lazy as LBS

-- | Generate all 2-splits of a string.
splits2 :: LBS.ByteString -> [[LBS.ByteString]]
splits2 bs = zipWith (\a b -> [a,b]) (LBS.inits bs) (LBS.tails bs)

-- | Generate all 3-splits of a string.
splits3 :: LBS.ByteString -> [[LBS.ByteString]]
splits3 bs =
  [ [a,b,c]
  |   (a,bs') <- zip (LBS.inits bs)  (LBS.tails bs)
    , (b,c  ) <- zip (LBS.inits bs') (LBS.tails bs')
  ]

-- | For 'f' function in Tests.
instance Show (Int -> Int) where
  show _ = ""
