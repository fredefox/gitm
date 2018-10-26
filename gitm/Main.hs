{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import qualified System.Environment as System
import Data.Functor (void)
import Data.Text (Text)
import qualified Data.Text as Text

import qualified Git.GitM as GitM

-- | Downloads all projects specified in the arguments.
main :: IO ()
main = getArgs >>= \case
  [] → GitM.cloneStoredProjects
  xs → void $ traverse GitM.github xs

getArgs ∷ IO [Text]
getArgs = map Text.pack <$> System.getArgs
