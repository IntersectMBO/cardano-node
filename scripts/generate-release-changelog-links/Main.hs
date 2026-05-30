module Main (main) where

import           Control.Monad (guard)
import           Data.Aeson
import           Data.ByteString.Char8 (ByteString)
import qualified Data.CaseInsensitive as CI
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import           Data.Version
import           Network.HTTP.Client (HttpException (..), HttpExceptionContent (..),
                   responseHeaders, responseStatus)
import           Network.HTTP.Req
import           Network.HTTP.Types.Header (hLocation)
import           Network.HTTP.Types.Status (found302)
import qualified Network.URI as URI
import qualified Network.URI.Encode as URIE
import           Options.Applicative
import           System.Environment (lookupEnv)
import           System.Exit (exitFailure)
import           System.IO (hPutStrLn, stderr)

import           Cabal.Plan
import qualified GitHub

die :: Text -> IO a
die msg = Text.hPutStrLn stderr msg >> exitFailure

main :: IO ()
main = do
  (outputPath, planJsonFilePath) <- execParser parserInfo

  gitHubAccessToken <-
    lookupEnv "GITHUB_TOKEN" >>= \case
      Nothing -> die "GITHUB_TOKEN environment variable is not set"
      Just t  -> pure $ GitHubAccessToken (Text.encodeUtf8 (Text.pack t))

  packagesMap <- getCHaPPackagesMap

  hPutStrLn stderr $ "Reading Cabal plan from " <> show planJsonFilePath
  versions <- selectPackageVersion planJsonFilePath

  changelogPaths <-
    concat
      <$> mapM (processVersion packagesMap gitHubAccessToken) (List.nub versions)

  let res = generateMarkdown changelogPaths
  Text.writeFile outputPath (res <> "\n")

processVersion
  :: PackagesMap
  -> GitHubAccessToken
  -> PkgId
  -> IO [(PkgName, Ver, Maybe (Text, Text))]
processVersion packagesMap gitHubAccessToken version@(PkgId n v) = do
  hPutStrLn stderr $ "Looking up CHaP entry for " <> show version
  mEntry <- lookupCHaPEntry version packagesMap
  case mEntry of
    Nothing -> return []
    Just chapEntry -> do
      hPutStrLn stderr $
        "Searching for CHANGELOG.md on GitHub for " <> show version
      changelogLocation <- findChangelogFromGitHub gitHubAccessToken chapEntry
      return [(n, v, changelogLocation)]

parserInfo :: ParserInfo (FilePath, FilePath)
parserInfo = info (argsParser <**> helper) $
     progDesc "Generate a Markdown changelog table for a cardano-node release"
  <> footer
       "Requires GITHUB_TOKEN in the environment. The token can be generated \
       \at https://github.com/settings/tokens, or retrieved via \
       \`gh auth token` after logging in with the GitHub CLI."

argsParser :: Parser (FilePath, FilePath)
argsParser =
  (,)
    <$> strOption
          (  long "output"
          <> short 'o'
          <> metavar "OUTPUT"
          <> help "Write the generated links to OUTPUT"
          )
    <*> argument str
          (  metavar "plan_json_path"
          <> help "Path of the plan.json file"
          <> value "result/plan.json"
          <> showDefault
          )

selectPackageVersion :: FilePath -> IO [PkgId]
selectPackageVersion planJsonFilePath = do
  cabalPlan <- eitherDecodeFileStrict planJsonFilePath >>= \case
    Left aesonError ->
      die $ "Failed to parse plan.json: " <> Text.pack aesonError
    Right res -> pure res
  return
    [ uPId
    | Unit{..} <- Map.elems (pjUnits cabalPlan)
    , isProbablyCHaP Unit{..}
    ]

hackageURI :: URI
hackageURI = URI "http://hackage.haskell.org/"

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
  { packageName    :: Text
  , packageVersion :: Version
  , packageURL     :: Text
  }
  deriving (Show, Eq, Ord)

instance FromJSON PackageDescription where
  parseJSON = withObject "PackageDescription" $ \obj ->
    PackageDescription <$> obj .: "pkg-name"
                       <*> obj .: "pkg-version"
                       <*> obj .: "url"

getCHaPPackages :: IO CHaPPackages
getCHaPPackages =
  fmap responseBody $ runReq defaultHttpConfig $
    req GET chapPackagesURL NoReqBody jsonResponse mempty

type PackagesMap = Map (Text, Version) Text

getCHaPPackagesMap :: IO PackagesMap
getCHaPPackagesMap = do
  CHaPPackages ps <- getCHaPPackages
  pure $ Map.fromList $
    map (\PackageDescription{..} -> ((packageName, packageVersion), packageURL))
        ps

chapPackagesURL :: Url 'Https
chapPackagesURL =
  https "chap.intersectmbo.org" /: "foliage" /: "packages.json"

lookupCHaPEntry :: PkgId -> PackagesMap -> IO (Maybe CHaPEntry)
lookupCHaPEntry (PkgId (PkgName n) (Ver v)) packagesMap =
  case Map.lookup (n, Version v []) packagesMap of
    Nothing -> return Nothing
    Just chapURL ->
      case parseCHaPEntry chapURL of
        Nothing -> do
          hPutStrLn stderr $
            "Skipping " <> show n
            <> " as its packages.json URL could not be parsed"
          return Nothing
        Just chapEntry -> return (Just chapEntry)

-- Parses CHaP package URLs of the form:
-- github:owner/repo/revision[?dir=subdir]
parseCHaPEntry :: Text -> Maybe CHaPEntry
parseCHaPEntry chapURL = do
  rest  <- Text.stripPrefix "github:" chapURL
  let (owner, rest1) = Text.breakOn "/" rest
  rest2 <- Text.stripPrefix "/" rest1
  let (repo, rest3) = Text.breakOn "/" rest2
  rest4 <- Text.stripPrefix "/" rest3
  let (rev, rest5) = Text.breakOn "?dir=" rest4
      subdir       = if Text.null rest5
                     then Nothing
                     else Just (Text.drop (Text.length "?dir=") rest5)
  guard $
    not (Text.null owner) && not (Text.null repo) && not (Text.null rev)
  pure $ CHaPEntry
    (GitHub.mkOwnerName owner)
    (GitHub.mkRepoName repo)
    rev
    subdir

data CHaPEntry = CHaPEntry
  { entryGitHubOwner    :: GitHub.Name GitHub.Owner
  , entryGitHubRepo     :: GitHub.Name GitHub.Repo
  , entryGitHubRevision :: Text
  , entrySubdir         :: Maybe Text
  }
  deriving (Show)

findChangelogFromGitHub
  :: GitHubAccessToken -> CHaPEntry -> IO (Maybe (Text, Text))
findChangelogFromGitHub accessToken c@CHaPEntry{..} = do
  print c
  let query = changelogLookupGitHub
                entryGitHubOwner entryGitHubRepo
                entrySubdir entryGitHubRevision
  contentDir <- runGitHub accessToken query >>= \case
    Left (GitHub.HTTPError
            originalError@(HttpExceptionRequest _
              (StatusCodeException resp _))) ->
      if responseStatus resp == found302
      then
        case List.lookup hLocation (responseHeaders resp) of
          Nothing ->
            die "findChangelogFromGitHub: HTTP 302 with no location header"
          Just redirectLocation -> do
            let loc = URIE.decodeText
                        $ Text.dropEnd 2
                        $ Text.decodeUtf8 redirectLocation
            newQuery <- case query of
              GitHub.Query _ queryString -> do
                segs <- generateRedirectPathSegments loc
                pure $ GitHub.query segs queryString
              _ ->
                die "findChangelogFromGitHub: expected a Query request type"
            runGitHub accessToken newQuery >>= \case
              Left e' -> die $ Text.unlines
                [ "Redirect failed: "  <> Text.pack (show e')
                , "Original error: "   <> Text.pack (show originalError)
                ]
              Right (GitHub.ContentFile _) ->
                die "Redirect result: expected a directory, got a file"
              Right (GitHub.ContentDirectory dir) -> pure dir
      else die $
        "GitHub lookup failed: " <> Text.pack (show resp)
    Left gitHubError ->
      die $ "GitHub lookup failed: " <> Text.pack (show gitHubError)
    Right (GitHub.ContentFile _) ->
      die "Expected a directory from changelogLookupGitHub, got a file"
    Right (GitHub.ContentDirectory dir) -> pure dir

  pure $ case Foldable.find looksLikeChangelog contentDir of
    Nothing  -> Nothing
    Just res ->
      let name = GitHub.contentName (GitHub.contentItemInfo res)
          path = GitHub.contentPath (GitHub.contentItemInfo res)
      in Just (name, constructGitHubPath
                       entryGitHubOwner entryGitHubRepo
                       entryGitHubRevision path)

generateRedirectPathSegments :: Text -> IO [Text]
generateRedirectPathSegments url =
  case URI.parseURI (Text.unpack url) of
    Just uri ->
      let segs = map Text.pack $ URI.pathSegments uri
      in if null segs
         then die $ "No path segments in URL: " <> url
         else return segs
    Nothing -> die $ "Invalid URL: " <> url

changelogLookupGitHub
  :: GitHub.Name GitHub.Owner
  -> GitHub.Name GitHub.Repo
  -> Maybe Text
  -> Text
  -> GitHub.Request k GitHub.Content
changelogLookupGitHub owner repo subdir revision =
  GitHub.contentsForR owner repo (fromMaybe "" subdir) (Just revision)

looksLikeChangelog :: GitHub.ContentItem -> Bool
looksLikeChangelog GitHub.ContentItem{..} =
  contentItemType == GitHub.ItemFile
  && CI.mk (GitHub.contentName contentItemInfo) == "CHANGELOG.md"

constructGitHubPath
  :: GitHub.Name GitHub.Owner
  -> GitHub.Name GitHub.Repo
  -> Text
  -> Text
  -> Text
constructGitHubPath owner repo revision path =
  "https://github.com/"
  <> GitHub.untagName owner <> "/"
  <> GitHub.untagName repo  <> "/blob/"
  <> revision               <> "/"
  <> path

newtype GitHubAccessToken = GitHubAccessToken ByteString
  deriving (Show, Eq, Ord)

runGitHub :: GitHub.GitHubRW req res => GitHubAccessToken -> req -> res
runGitHub (GitHubAccessToken tok) = GitHub.github (GitHub.OAuth tok)

generateMarkdown :: [(PkgName, Ver, Maybe (Text, Text))] -> Text
generateMarkdown changelogPaths =
  Text.unlines $ "Package changelogs" : "" : render rows
  where
    rows = mkHeader : map mkRow changelogPaths

    mkHeader                        = ["Package", "Version", "Changelog"]
    mkRow (PkgName n, v, linkMaybe) = [n, dispVer v, dispLink linkMaybe]

    dispLink (Just (file, link)) =
      "[" <> file <> "](" <> link <> " \"" <> file <> "\")"
    dispLink Nothing = ""

    render :: [[Text]] -> [Text]
    render =
      map renderRow
      . List.transpose
      . map (separator . innerMargins . alignLeft)
      . List.transpose

    renderRow = surroundWith '|' . Text.intercalate "|"

    alignLeft ts =
      let maxLen = maximum (Text.length <$> ts)
      in map (Text.justifyLeft maxLen ' ') ts

    surroundWith c = Text.cons c . flip Text.snoc c
    innerMargins   = map (surroundWith ' ')

    separator (h:rs) = h : Text.replicate (Text.length h) "-" : rs
    separator []     = []
