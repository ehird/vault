module Data.Vault.Key (
    Key, newKey,
    ) where

import qualified Data.Vault.Key.ST as ST
import Control.Monad.ST

-- | Keys to access a vault or a universe.
--
-- This variant is the simplest and creates keys in the 'IO' monad.
-- See the module "Data.Vault.Key.ST" if you want to use it with the 'ST' monad instead.
--
-- > Key :: * -> *
type Key = ST.Key RealWorld

-- | Create a new key for use with a vault or a universe.
newKey :: IO (Key a)
newKey = stToIO ST.newKey
