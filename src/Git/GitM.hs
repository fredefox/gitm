{-# Language TemplateHaskell, OverloadedLists, RecordWildCards #-}
-- {-# OPTIONS_GHC -Wall -Wcompat #-}

module Git.GitM (cloneStoredProjects, github, clone, LocSpec(..), scan) where

import Prelude (dropWhile, break, words, foldr)
import Control.Monad (filterM)

import System.Directory
import Control.Monad (join)
import Frelude
import Data.Functor (void)
import qualified Shelly
import qualified Data.Yaml as Yaml
import Data.ByteString (ByteString)
import Data.Text.Prettyprint.Doc.Render.Text (putDoc)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.FileEmbed (embedFile)
import Git.GitM.Url
import Data.Maybe (catMaybes)
import Data.Aeson
import Data.Aeson.Types
import Data.Vector (Vector)
import qualified Data.Vector as Vector

import Debug.Trace

projects ∷ ByteString
projects = $(embedFile "data/projects.yaml")

clone ∷ [Text] → IO ()
clone s
  = Shelly.shelly
  $ Shelly.errExit False
  $ Shelly.verbosely
  $ Shelly.run_ "git"
  $ ["clone"] <> s

-- TODO Make configurable.
url ∷ Text
url = "git@gitlab.com"
-- url = "git@github.com"
-- url = "git@localhost"

github ∷ Text → IO ()
github s = clone $ [url <:> s, s]

getProjects ∷ MonadThrow m ⇒ m (HashMap Text [Text])
getProjects = Yaml.decodeThrow projects

cloneStoredProjects ∷ IO ()
cloneStoredProjects = do
  ps ← getProjects
  putDoc $ prettyHashMap ps
  void $ HashMap.traverseWithKey dlUser ps

prettyHashMap ∷ HashMap Text [Text] → Doc a
prettyHashMap = pretty . convertString @ByteString @Text . Yaml.encode

dlUser ∷ Text → [Text] → IO ()
dlUser usr = void . traverse step
  where
  step ∷ Text → IO ()
  step rpo = github $ usr </> rpo

data LocSpec = LocSpec
  { user ∷ Text
  , repo ∷ Text
  }

instance FromJSON LocSpec where
  parseJSON = withObject "loc-spec"
    $ \[(k, v)] → LocSpec <$> pure k <*> (parseJSON v)
  parseJSONList = withObject "loc-spec" (pure . step)
    where
    step ∷ Object → [LocSpec]
    step = HashMap.foldrWithKey go mempty
    go ∷ Text → Value → [LocSpec] → [LocSpec]
    go user v = case v of
      (String repo) → mappend [LocSpec user repo]
      (Array repos) →
        mappend (Vector.toList $ map (\(String repo) → LocSpec user repo) repos)

instance ToJSON LocSpec where
  toJSON LocSpec{..} = object [ user .= repo ]
  toJSONList = Object . map Array . foldr step mempty
    where
    step ∷ LocSpec → HashMap Text Array → HashMap Text Array
    step LocSpec{..} = HashMap.insertWith mappend user (pure $ String repo)

deriving stock instance Show LocSpec

scan ∷ FilePath → IO [LocSpec]
scan (convertString → p) = do
  usrs ← subdirs p
  join <$> traverse perUser usrs
  where
  perUser ∷ FilePath → IO [LocSpec]
  perUser usr = do
    rpos ← subdirs usr
    pure $ catMaybes $ map perRepo rpos
  perRepo ∷ FilePath → Maybe LocSpec
  perRepo rpo = case reverse $ segs rpo of
    (repo : user : _) → pure $ LocSpec (convertString user) (convertString repo)
    _ → Nothing

segs ∷ String → [String]
segs s = case dropWhile isSlash s of
  "" -> []
  s' -> w : segs s''
        where (w, s'') =
               break isSlash s'
  where
  isSlash ∷ Char → Bool
  isSlash = (== '/')
                                             
subdirs ∷ FilePath → IO [FilePath]
subdirs root = do
  c ← map (\p → root <> p </> "") <$> listDirectory root
  filterM doesDirectoryExist c
