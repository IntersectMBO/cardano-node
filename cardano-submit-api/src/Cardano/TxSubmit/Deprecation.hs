{-# LANGUAGE OverloadedStrings #-}

module Cardano.TxSubmit.Deprecation
  ( guideUrl
  , deprecationNotice
  , printDeprecationBanner
  , deprecationMiddleware
  ) where

import qualified Data.ByteString.Char8 as BS8
import           Data.String (fromString)
import           Network.HTTP.Types (Header)
import           Network.Wai (Middleware, mapResponseHeaders)
import           System.IO (stderr)

-- | Write the banner to stderr, bypassing the tracer, which users can silence.
printDeprecationBanner :: IO ()
printDeprecationBanner = BS8.hPutStr stderr $ BS8.pack banner

-- | Mark every response as deprecated (RFC 9745).
deprecationMiddleware :: Middleware
deprecationMiddleware application request respond =
  application request $ respond . mapResponseHeaders (deprecationHeaders <>)

guideUrl :: String
guideUrl = "https://github.com/IntersectMBO/cardano-node/blob/master/cardano-submit-api/DEPRECATION.md"

deprecationNotice :: [String]
deprecationNotice =
  [ "cardano-submit-api is deprecated and will be removed in a future release."
  , "Use the UTxO RPC interface built into cardano-node (cardano-rpc) instead."
  ]

banner :: String
banner =
  unlines $
    [frame, "# WARNING: cardano-submit-api IS DEPRECATED", "#"]
      <> fmap ("# " <>) deprecationNotice
      <> ["#", "# Migration guide:", "# " <> guideUrl, frame]
  where
    frame = replicate 78 '#'

deprecationHeaders :: [Header]
deprecationHeaders =
  [ ("Deprecation", "@1792368000")
  , ("Link", "<" <> fromString guideUrl <> ">; rel=\"deprecation\"")
  ]
