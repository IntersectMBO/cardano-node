#!/usr/bin/env -S cabal run --verbose=1 --index-state=2023-10-04T00:00:00Z
{- cabal:
  build-depends:
    base,
    aeson,
    bytestring,
    cabal-plan,
    case-insensitive,
    containers,
    foldl,
    github ^>= 0.28,
    optparse-applicative ^>= 0.18,
    ansi-wl-pprint >= 1,
    pandoc ^>= 3.1,
    prettyprinter,
    req,
    text,
    turtle ^>= 1.6.0,
  default-extensions:
    BlockArguments,
    DataKinds,
    ImportQualifiedPost,
    LambdaCase,
    OverloadedStrings,
    RecordWildCards
  ghc-options: -Wall -Wextra -Wcompat
-}

-- `nix build .#project.x86_64-linux.plan-nix.json` is a reliable way to
-- generate the `plan.json` to be fed to this script.

module Main (main) where

import           Cabal.Plan
import qualified Control.Foldl as Foldl
import           Data.Aeson
import           Data.ByteString.Char8 (ByteString)
import qualified Data.CaseInsensitive as CI
import           Data.Foldable
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import qualified Data.Text.Encoding as Text
import           Data.Version
import qualified GitHub
import           Network.HTTP.Req
import           Options.Applicative
import           Prettyprinter
import qualified Prettyprinter.Util as PP
import qualified Text.Pandoc as Pandoc
import           Turtle

main :: IO ()
main = sh do

  (planJsonFilePath, gitHubAccessToken) <-
    options generateReleaseChangelogLinksDescription do
      (,) <$> argPath "plan_json_path" "Path of the plan.json file"
          <*> fmap (GitHubAccessToken . Text.encodeUtf8) (argText "github_access_token" "GitHub personal access token")

  packagesMap <- getCHaPPackagesMap

  changelogPaths <- reduce Foldl.list do

    -- find all of the packages in the plan.json that are hosted on CHaP
    printf ("Reading Cabal plan from "%w%"\n") planJsonFilePath
    version@(PkgId n v) <- nub $ selectPackageVersion planJsonFilePath

    -- from cardano-haskell-packages, retrieve the package repo / commit / subdir
    printf ("Looking up CHaP entry for "%repr version%"\n")
    chapEntry <- lookupCHaPEntry version packagesMap

    -- from github, get the package's CHANGELOG.md location
    printf ("Searching for CHANGELOG.md on GitHub for "%repr version%"\n")
    changelogLocation <- findChangelogFromGitHub gitHubAccessToken chapEntry

    pure (n, v, changelogLocation)

  -- generate a massive markdown table
  let writerOptions =
        Pandoc.def { Pandoc.writerExtensions = Pandoc.githubMarkdownExtensions }
      pandocOutput = Pandoc.runPure do
        Pandoc.writeMarkdown writerOptions (generatePandoc changelogPaths)

  case pandocOutput of
    Left pandocError -> die $
      "Failed to render markdown with error " <> Pandoc.renderError pandocError
    Right res -> printf (s%"\n") res

generateReleaseChangelogLinksDescription :: Description
generateReleaseChangelogLinksDescription = Description $
  mconcat
    [ "generate-release-changelog-links.hs"
    , line, line
    , fillSep $ PP.words
        "This script requires a GitHub personal access token, which can be \
        \generated either at https://github.com/settings/tokens or retrieved \
        \using the GitHub CLI tool with `gh auth token` (after logging in)"
    ]

selectPackageVersion :: FilePath -> Shell PkgId
selectPackageVersion planJsonFilePath = do
  cabalPlan <- liftIO do
    eitherDecodeFileStrict planJsonFilePath >>= \case
      Left aesonError ->
        die $ "Failed to parse plan.json: " <> fromString aesonError
      Right res -> pure res

  Unit{..} <- select (pjUnits cabalPlan)

  -- we only care about packages which are hosted on CHaP
  guard (isProbablyCHaP Unit{..})

  pure uPId

hackageURI :: URI
hackageURI =
  URI "http://hackage.haskell.org/"

isProbablyCHaP :: Unit -> Bool
isProbablyCHaP Unit{..} =
  case uPkgSrc of
    Just (RepoTarballPackage (RepoSecure repoUri)) -> repoUri /= hackageURI
    _ -> False

newtype CHaPPackages = CHaPPackages [PackageDescription]
  deriving (Show, Eq, Ord)

instance FromJSON CHaPPackages where
  parseJSON v = CHaPPackages <$> parseJSON v

data PackageDescription = PackageDescription
  { packageName :: Text
  , packageVersion :: Version
  , packageURL :: Text
  }
  deriving (Show, Eq, Ord)

instance FromJSON PackageDescription where
  parseJSON = withObject "PackageDescription" $ \obj -> do
    PackageDescription <$> obj .: "pkg-name"
                       <*> obj .: "pkg-version"
                       <*> obj .: "url"

getCHaPPackages :: MonadIO m => m CHaPPackages
getCHaPPackages = do
  fmap responseBody $ liftIO $ runReq defaultHttpConfig $
    req GET chapPackagesURL NoReqBody jsonResponse mempty

type PackagesMap = Map (Text, Version) Text

getCHaPPackagesMap :: MonadIO m => m PackagesMap
getCHaPPackagesMap = do
  CHaPPackages ps <- getCHaPPackages
  pure $ Map.fromList $
    map (\PackageDescription{..} -> ((packageName, packageVersion), packageURL)) ps

chapPackagesURL :: Url 'Https
chapPackagesURL =
  https "chap.intersectmbo.org" /: "foliage" /: "packages.json"

lookupCHaPEntry :: PkgId -> PackagesMap -> Shell CHaPEntry
lookupCHaPEntry (PkgId (PkgName n) (Ver v)) packagesMap = do
  chapURL <- maybe empty pure $ Map.lookup (n, Version v []) packagesMap

  case match packagesJSONUrlPattern chapURL of
    [] -> do
      printf ("Skipping "%repr n%" as its packages.json URL could not be parsed\n")
      empty
    chapEntry : _ ->
      pure chapEntry

-- parses something like this:
-- github:input-output-hk/cardano-ledger/760a73e89ef040d3ad91b4b0386b3bbace9431a9?dir=eras/byron/ledger/executable-spec
packagesJSONUrlPattern :: Pattern CHaPEntry
packagesJSONUrlPattern = do
  void "github:"
  owner <- plus (alphaNum <|> char '-')
  void "/"
  repo <- plus (alphaNum <|> char '-')
  void "/"
  revision <- plus hexDigit
  subdir <- optional do
    void "?dir="
    plus (alphaNum <|> char '.' <|> char '/' <|> char '-')
  eof
  pure $ CHaPEntry (GitHub.mkOwnerName owner) (GitHub.mkRepoName repo) revision subdir

data CHaPEntry =
  CHaPEntry { entryGitHubOwner :: GitHub.Name GitHub.Owner
            , entryGitHubRepo :: GitHub.Name GitHub.Repo
            , entryGitHubRevision :: Text
            , entrySubdir :: Maybe Text
            }
  deriving (Show)

findChangelogFromGitHub :: MonadIO m => GitHubAccessToken -> CHaPEntry -> m (Maybe (Text, Text))
findChangelogFromGitHub accessToken CHaPEntry{..} = do
  contentDir <- liftIO (runGitHub accessToken (changelogLookupGitHub entryGitHubOwner entryGitHubRepo entrySubdir entryGitHubRevision)) >>= \case
    Left gitHubError -> die $
      "GitHub lookup failed with error " <> repr gitHubError
    Right (GitHub.ContentFile _) -> die
      "Expected changelogLookupGitHub to return a directory, but got a single file"
    Right (GitHub.ContentDirectory dir) -> pure dir

  pure $ case Data.Foldable.find looksLikeChangelog contentDir of
    Nothing -> Nothing
    Just res -> do
      let name = GitHub.contentName (GitHub.contentItemInfo res)
          path = GitHub.contentPath (GitHub.contentItemInfo res)
      Just (name, constructGitHubPath entryGitHubOwner entryGitHubRepo entryGitHubRevision path)

changelogLookupGitHub :: GitHub.Name GitHub.Owner
                      -> GitHub.Name GitHub.Repo
                      -> Maybe Text
                      -> Text
                      -> GitHub.Request k GitHub.Content
changelogLookupGitHub owner repo subdir revision =
  GitHub.contentsForR owner repo (fromMaybe "" subdir) (Just revision)

looksLikeChangelog :: GitHub.ContentItem -> Bool
looksLikeChangelog GitHub.ContentItem{..} = do
  let caseInsensitiveName = CI.mk (GitHub.contentName contentItemInfo)
  contentItemType == GitHub.ItemFile && caseInsensitiveName == "CHANGELOG.md"

constructGitHubPath :: GitHub.Name GitHub.Owner
                    -> GitHub.Name GitHub.Repo
                    -> Text
                    -> Text
                    -> Text
constructGitHubPath =
  format ("https://github.com/"%ghname%"/"%ghname%"/blob/"%s%"/"%s)
  where
    ghname = makeFormat GitHub.untagName

newtype GitHubAccessToken = GitHubAccessToken ByteString
  deriving (Show, Eq, Ord)

runGitHub :: GitHub.GitHubRW req res => GitHubAccessToken -> req -> res
runGitHub (GitHubAccessToken tok) =
    GitHub.github (GitHub.OAuth tok)

generatePandoc :: [(PkgName, Ver, Maybe (Text, Text))] -> Pandoc.Pandoc
generatePandoc ps =
  Pandoc.Pandoc mempty
    [ Pandoc.Plain [Pandoc.Str "Package changelogs"]
    , Pandoc.Table mempty (Pandoc.Caption Nothing []) colSpec tableHead [tableBody] (Pandoc.TableFoot mempty mempty)
    ]
  where
    colSpec = replicate 3 (Pandoc.AlignDefault, Pandoc.ColWidthDefault)
    tableHead = Pandoc.TableHead mempty [Pandoc.Row mempty tableHeadCells]
    tableHeadCells =
      [ mkCell [Pandoc.Str "Package"]
      , mkCell [Pandoc.Str "Version"]
      , mkCell [Pandoc.Str "Changelog"]
      ]
    tableBody = Pandoc.TableBody mempty 0 [] (fmap mkTableRow ps)
    mkTableRow (PkgName n, v, linkMaybe) =
      Pandoc.Row mempty
        [ mkCell [Pandoc.Str n]
        , mkCell [Pandoc.Str (dispVer v)]
        , mkCell (foldMap (\(fn, link) -> [Pandoc.Link mempty [Pandoc.Str fn] (link, fn)]) linkMaybe)
        ]
    mkCell t = Pandoc.Cell mempty Pandoc.AlignDefault 1 1 [Pandoc.Plain t]
