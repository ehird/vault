{-----------------------------------------------------------------------------
    Vault
    
    A typed, persistent store for values of arbitrary types
    
    This implementation uses  unsafeCoerce  for reasons of efficiency.
    See  http://apfelmus.nfshost.com/blog/2011/09/04-vault.html
    for an implementation that doesn't need to bypass the type checker.
------------------------------------------------------------------------------}
module Data.Vault (
    module Data.Vault.Key,
    Vault,
    empty, lookup, insert, adjust, delete, union,
    ) where

import Prelude hiding (lookup)
import Control.Monad.ST
import Data.Vault.Key
import qualified Data.Vault.ST as ST

-- | A typed, persistent store for values of arbitrary types.
-- 
-- This variant is the simplest and creates keys in the 'IO' monad.
-- See the module "Data.Vault.ST" if you want to use it with the 'ST' monad instead.
--
-- > instance Monoid Vault
type Vault = ST.Vault RealWorld

-- | The empty vault.
empty :: Vault
empty = ST.empty

-- | Lookup the value of a key in the vault.
lookup :: Key a -> Vault -> Maybe a
lookup = ST.lookup

-- | Insert a value for a given key. Overwrites any previous value.
insert :: Key a -> a -> Vault -> Vault
insert = ST.insert

-- | Adjust the value for a given key if it's present in the vault.
adjust :: (a -> a) -> Key a -> Vault -> Vault
adjust = ST.adjust

-- | Delete a key from the vault.
delete :: Key a -> Vault -> Vault
delete = ST.delete

-- | Merge two vaults (left-biased).
union :: Vault -> Vault -> Vault
union = ST.union
