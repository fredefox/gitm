{-# OPTIONS_GHC -Wall -Wcompat #-}
{-# Language OverloadedStrings, RecordWildCards, OverloadedLists #-}

module GitM.Backend
  ( Command(..)
  , getCommand
  , Clone(..)
  , Scan(..)
  , Backend(..)
  , Spec(..)
  , Url(..)
  , backends
  ) where

import Prelude
import Frelude
import Options.Applicative (Parser, ParserInfo, (<**>))
import qualified Options.Applicative as O
import qualified GitM.Version as Version
import Data.HashMap.Strict (HashMap)
import GitM.Url

data Spec = Spec
  { user ∷ Text
  , repo ∷ Text
  }

data Backend = UnknownBackend Text | Backend
  { fromUrl  ∷ Url  → Spec
  , fromSpec ∷ Spec → Url
  , name     ∷ Text
  }

backend ∷ Text → Text → Backend
backend base name = Backend
  { fromUrl = error "TODO: Not implemented"
  , fromSpec = \Spec{..} → Url (base <:> user </> repo <.> "git")
  , name = name
  }

github ∷ Backend
github = backend "git@github.com" "GitHub"

gitlab ∷ Backend
gitlab = backend "git@gitlab.com" "GitLab"

backends ∷ HashMap Text Backend
backends =
  [ "github" |> github
  , "gitlab" |> gitlab
  ]
  where
  (|>) = (,)

data Command = CloneCmd Clone | ScanCmd Scan

data Clone = Clone Text

data Scan = Scan

getCommand ∷ IO Command
getCommand
  = O.execParser
  $ O.info (commandParser <**> O.helper <**> version)
  (  O.fullDesc
  <> O.progDesc descr
  <> O.header header
  )
  where
  header ∷ String
  header = "Utility for managing multiple git projects"
  -- TODO Something better here.
  descr ∷ String
  descr = header


version ∷ Parser (a → a)
version = O.infoOption Version.version $ mconcat
  [ O.long "version"
  , O.help "Output version information and exit"
  , O.hidden
  ]

commandParser ∷ Parser Command
commandParser = O.subparser
  (  O.command "clone" (CloneCmd <$> cloneParser)
  <> O.command "scan"  (ScanCmd  <$> scanParser)
  )

cloneParser ∷ ParserInfo Clone
cloneParser = Clone <$> O.info pathParser (O.header "Clone a repo")

pathParser ∷ Parser Text
pathParser = arg "PATH"
  
arg ∷ IsString s ⇒ String → Parser s
arg m = O.strArgument (O.metavar m)

-- TODO Stub
scanParser ∷ ParserInfo Scan
scanParser = Scan <$ O.info empty (O.header "Scan all repos")
