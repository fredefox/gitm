module Git.GitM.Url
  ( Url(..)
  , sepBy
  , (<:>)
  , (</>)
  , (<->)
  , (<.>)
  ) where

import Frelude

newtype Url = Url Text
deriving newtype instance IsString Url
deriving newtype instance Semigroup Url

sepBy ∷ Semigroup s ⇒ IsString s ⇒ s → s → s → s
sepBy x a b = a <> x <> b

(<:>) ∷ Semigroup s ⇒ IsString s ⇒ s → s → s
(<:>) = sepBy ":"

(</>) ∷ Semigroup s ⇒ IsString s ⇒ s → s → s
(</>) = sepBy "/"

(<->) ∷ Semigroup s ⇒ IsString s ⇒ s → s → s
(<->) = sepBy " "

(<.>) ∷ Semigroup s ⇒ IsString s ⇒ s → s → s
(<.>) = sepBy "."
