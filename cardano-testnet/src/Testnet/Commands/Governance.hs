

module Testnet.Commands.Governance
  ( createByronUpdateProposal
  ) where

import           Prelude

import           Control.Monad.Catch
import           Control.Monad.IO.Class

import           Testnet.Util.Process (execCli_)

import           Hedgehog.Internal.Property


createByronUpdateProposal
  :: (MonadTest m, MonadCatch m, MonadIO m)
  => Int -> String -> String -> Int -> m ()
createByronUpdateProposal testnetMagic signingKeyFp updateProposalFp ptclMajorVersion =
  execCli_
    [ "byron", "governance", "create-update-proposal"
    , "--filepath", updateProposalFp
    , "--testnet-magic", show testnetMagic
    , "--signing-key", signingKeyFp
    , "--protocol-version-major", show ptclMajorVersion
    , "--protocol-version-minor", "0"
    , "--protocol-version-alt", "0"
    , "--application-name", "cardano-sl"
    , "--software-version-num", "1"
    , "--system-tag", "linux"
    , "--installer-hash", "0"
    ]
