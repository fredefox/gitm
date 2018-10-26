{-# Language TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}
module Git.GitM (cloneStoredProjects, github) where

import Data.Functor (void)
import qualified Shelly
import Data.Text (Text)
import qualified Data.Yaml as Yaml
import Data.ByteString (ByteString)
import Data.Text.Prettyprint.Doc (Pretty(pretty), Doc)
import Data.Text.Prettyprint.Doc.Render.Text (putDoc)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.FileEmbed (embedFile)
import Control.Monad.Catch (MonadThrow)
import Data.String.Conversions (convertString)

projects ∷ ByteString
projects = $(embedFile "data/projects.yaml")

git ∷ [Text] → IO ()
git s
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
github s = git $ [url <:> s, s]

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
  step repo = github $ usr </> repo

(</>) ∷ Text → Text → Text
a </> b = a <> "/" <> b

(<:>) ∷ Text → Text → Text
a <:> b = a <> ":" <> b
