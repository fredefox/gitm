{-# OPTIONS_GHC -Wall -Wcompat #-}
{-# Language OverloadedStrings, RecordWildCards, OverloadedLists #-}

module GitM.Options
  ( Command(..)
  , getCommand
  , Clone(..)
  , Scan(..)
  ) where

import Prelude
import Frelude
import Options.Applicative (Parser, ParserInfo, (<**>))
import qualified Options.Applicative as O
import qualified GitM.Version as Version

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
scanParser = Scan <$ O.info (pure ()) (O.header "Scan all repos")
