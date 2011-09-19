{-----------------------------------------------------------------------------
    Universe
    
    A typed, persistent store for a single value of arbitrary type
------------------------------------------------------------------------------}
module Data.Universe (
    module Data.Vault.Key,
    Universe,
    toUniverse, fromUniverse,
    ) where

import Control.Monad.ST
import Data.Vault.Key
import qualified Data.Universe.ST as ST

-- | A typed, persistent store for a single value of arbitrary type.
-- 
-- This variant is the simplest and creates keys in the 'IO' monad.
-- See the module "Data.Universe.ST" if you want to use it with the 'ST' monad instead.
type Universe = ST.Universe RealWorld

-- | Create a universe and the key with which to access the value
-- inside.
toUniverse :: a -> IO (Key a, Universe)
toUniverse a = stToIO (ST.toUniverse a)

-- | Retrieve the value from a universe; this returns 'Nothing' if the
-- key is not the one provided upon creation of the universe.
fromUniverse :: Key a -> Universe -> Maybe a
fromUniverse = ST.fromUniverse
