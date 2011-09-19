{-# OPTIONS_HADDOCK hide #-}

module Data.Vault.Key.Internal (
    Key(..), newKey,
    ) where

import Data.Unique
import Control.Applicative
import Control.Monad.ST

newtype Key s a = Key Unique

newKey :: ST s (Key s a)
newKey = Key <$> unsafeIOToST newUnique
