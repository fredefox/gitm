module GitM.Version (version) where

import Frelude
import qualified Paths_gitm as Paths
import qualified Data.Version      as Version

version ∷ String
version = Version.showVersion Paths.version
