module Data.Vault.Key.ST (
    Key(..), newKey,
    ) where

import Control.Applicative
import Control.Monad.ST
import qualified Data.Vault.Key.Internal as I

-- | Keys to access a vault or a universe.
--
-- This variant has more complex types so that you can create keys in the 'ST' monad.
-- See the module "Data.Vault.Key" if you'd like to use a simpler version with the 'IO' monad.
-- You can also use both variants simultaneously; they share a single representation.
newtype Key s a = Key (I.Key s a)

-- | Create a new key for use with a vault or a universe.
newKey :: ST s (Key s a)
newKey = Key <$> I.newKey
