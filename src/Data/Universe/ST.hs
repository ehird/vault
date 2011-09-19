{-----------------------------------------------------------------------------
    Universe
    
    A typed, persistent store for a single value of arbitrary type
------------------------------------------------------------------------------}
module Data.Universe.ST (
    module Data.Vault.Key.ST,
    Universe,
    toUniverse, fromUniverse,
    ) where

import Data.Unique
import GHC.Exts (Any)   -- ghc specific tricks
import Unsafe.Coerce (unsafeCoerce)
import Control.Monad.ST
import qualified Data.Vault.Key.Internal as I
import Data.Vault.Key.ST

-- | A typed, persistent store for a single value of arbitrary type.
-- 
-- This variant has more complex types so that you can create keys in the 'ST' monad.
-- See the module "Data.Universe" if you'd like to use a simpler version with the 'IO' monad.
-- You can also use both variants simultaneously; they share a single representation.
data Universe s = Universe !Unique Any

-- | Create a universe and the key with which to access the value
-- inside.
toUniverse :: a -> ST s (Key s a, Universe s)
toUniverse a = do
    key@(Key (I.Key k)) <- newKey
    return (key, Universe k (unsafeCoerce a))

-- | Retrieve the value from a universe; this returns 'Nothing' if the
-- key is not the one provided upon creation of the universe.
fromUniverse :: Key s a -> Universe s -> Maybe a
fromUniverse (Key (I.Key k)) (Universe k' a)
    | k == k' = Just (unsafeCoerce a)
    | otherwise = Nothing
