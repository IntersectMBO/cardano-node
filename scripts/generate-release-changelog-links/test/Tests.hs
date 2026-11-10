module Tests (main) where

import           Control.Monad (unless)
import           Data.Either (isLeft)
import           Data.Maybe (isNothing)

import qualified GitHub
import qualified Main as Release

main :: IO ()
main = do
  let check label ok = unless ok (fail label)
  check "CHaP monorepo URL" $
    case Release.parseCHaPEntry "github:IntersectMBO/cardano-api/abcdef?dir=cardano-api" of
      Just entry ->
        GitHub.untagName (Release.entryGitHubOwner entry) == "IntersectMBO"
        && GitHub.untagName (Release.entryGitHubRepo entry) == "cardano-api"
        && Release.entryGitHubRevision entry == "abcdef"
        && Release.entrySubdir entry == Just "cardano-api"
      Nothing -> False
  check "CHaP repository root" $
    case Release.parseCHaPEntry "github:IntersectMBO/cardano-addresses/abcdef" of
      Just entry -> isNothing (Release.entrySubdir entry)
      Nothing -> False
  check "reject missing revision" $
    isNothing (Release.parseCHaPEntry "github:IntersectMBO/cardano-api/")
  check "retain complete redirect path" $
    Release.redirectPathSegments "https://api.github.com/repos/new/repo/contents/cardano-api"
      == Right ["repos", "new", "repo", "contents", "cardano-api"]
  check "separate redirect query from path" $
    Release.redirectPathSegments "https://api.github.com/repositories/123/contents/cardano-api?ref=abcdef"
      == Right ["repositories", "123", "contents", "cardano-api"]
  check "decode path components without splitting escaped slashes" $
    Release.redirectPathSegments "https://api.github.com/repos/new/repo/contents/a%2Fb%20c"
      == Right ["repos", "new", "repo", "contents", "a/b c"]
  check "reject malformed redirect" $
    isLeft (Release.redirectPathSegments "not a URL")
  check "reject empty redirect path" $
    isLeft (Release.redirectPathSegments "https://api.github.com")
  putStrLn "All 8 release changelog regression checks passed."
