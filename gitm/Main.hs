-- {-# OPTIONS_GHC -Wall -Wcompat #-}
{-# Language OverloadedStrings, RecordWildCards, OverloadedLists, NamedFieldPuns #-}

module Main (main) where

import Frelude
import qualified System.Environment as System
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.HashMap.Strict as HashMap
import Network.URI (URI(..))
import qualified Network.URI as URI
import Data.Maybe (catMaybes)

import Git.GitM (LocSpec(..))
import qualified Git.GitM as GitM
import qualified GitM.Options as Options
import Git.GitM.Url
import qualified Data.Yaml as Yaml
import qualified Data.ByteString.Char8 as ByteString

import Debug.Trace

-- | Downloads all projects specified in the arguments.
main :: IO ()
-- main = Options.test >>= print
main = Options.getCommand >>= \case
  Options.CloneCmd c → clone c
  Options.ScanCmd s → scan s

clone ∷ Options.Clone → IO ()
clone (Options.Clone rawUrl) = case manage rawUrl of
  Nothing → Text.putStrLn "I don't understand you.  It's my fault, it's not you."
  Just man → do
    cloneTo man
    -- Rebuild the database of managed projects
    scan Options.Scan
  where

data Manager = Manager
  { rawUrl        ∷ Text
  , normalizedUrl ∷ URI
  , locSpec       ∷ LocSpec
  }

manage ∷ Text → Maybe Manager
manage rawUrl = do
  normalizedUrl ← normalizeUrl rawUrl
  locSpec ← getLoc normalizedUrl
  pure $ Manager
    { rawUrl
    , normalizedUrl
    , locSpec
    }

normalizeUrl ∷ Text → Maybe URI
normalizeUrl rawSpec = listToMaybe $ catMaybes $
  [ verbatim
  , fromSCPSyntax
  ]
  where
  verbatim = URI.parseURI $ convertString rawSpec
  -- Some URLs are written using the SSH/SCP syntax that deviates from
  -- the URI RFC.
  fromSCPSyntax =
    if Text.any (== ':') rawSpec
    then URI.parseURI $ convertString $ "ssh://" <> Text.replace ":" "/" rawSpec
    else Nothing

cloneTo ∷ Manager → IO ()
cloneTo Manager{..} = do
  print locSpec
  printf "git clone %s %s\n" rawUrl fp
  GitM.clone [rawUrl, convertString fp]
  where
  fp = githome </> convertString user </> convertString repo
  LocSpec{..} = locSpec

-- TODO Stub
-- TODO The usage of the self-rolled '(</>)' is quite brittle.
home ∷ FilePath
home = "/home/fredefox"

githome ∷ FilePath
githome = home </> "git"

-- | This is not super elegant, but we assume the URL looks something
-- like this: @git://git@example.org/user/repo.git@.  We then proceed
-- to strip off the @.git@ suffix, then we inspect the path, split on
-- the @/@s, this should
getLoc ∷ URI → Maybe LocSpec
getLoc uri = case URI.pathSegments uri of
  [user, repo] → pure $ LocSpec
    { user = convertString user
    , repo = Text.replace ".git" "" $ convertString repo
    }
  _ → Nothing

projectsConfig ∷ FilePath
projectsConfig = "projects.yaml"

scan ∷ Options.Scan → IO ()
scan _ = do
  repos ← GitM.scan githome
  ByteString.writeFile projectsConfig (Yaml.encode repos)
