module Testnet.Utils where

import qualified Control.Lens as Lens
import           Control.Lens ((&), (.~), lensField, lensRules)
import           Language.Haskell.TH (mkName, nameBase)

-- | Custom rules for generating lenses for records without underscore-prefixed fields:
-- Instead of removing a prefix underscore, this rule adds a suffix one.
customLensRules :: Lens.LensRules
customLensRules = lensRules
  & lensField .~ (\_ _ field -> [Lens.TopName $ mkName (nameBase field ++ "_")])
