module Cardano.Api.Ledger.Mary
  ( PolicyID(..)
  ) where

import           Data.Aeson (ToJSON(..), ToJSONKey(..))
import           Data.Text (Text)
import           Prelude ((.))

import qualified Cardano.Ledger.Mary.Value as Mary
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Text.Encoding as Text
import qualified Cardano.Ledger.Shelley.API as Shelley
import qualified Cardano.Crypto.Hash.Class as Crypto

newtype PolicyID era = PolicyID (Mary.PolicyID era)

instance ToJSON (PolicyID era) where
  toJSON (PolicyID (Mary.PolicyID (Shelley.ScriptHash h))) = Aeson.String (hashToText h)

instance ToJSONKey (PolicyID era) where
  toJSONKey = Aeson.toJSONKeyText render
    where
      render (PolicyID (Mary.PolicyID (Shelley.ScriptHash h))) = hashToText h

hashToText :: Crypto.Hash crypto a -> Text
hashToText = Text.decodeLatin1 . Crypto.hashToBytesAsHex
