{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Blueprint.Lens
  (
    -- * Lenses for tables
    vlens
  , HasFLens(..)
    -- * Lenses for maps
  , HasKeyAt(..)
  , MapFind
  ) where

import           Data.Kind              (Type)
import           GHC.TypeLits           (Symbol)

import           Control.Lens.Lens

import           Blueprint.Internal.Map
import           Blueprint.Labels
import           Blueprint.Record

import           Typemap
import           Typemap.Lens


--------------------------------------------------------------------------------
--  Lenses for tables
--------------------------------------------------------------------------------

class HasFLens (record :: (u -> Type) -> t -> Type) where
  {-|

  > flens :: (HasKeyAt k m r a) => BlueKey k -> Lens' (Rec f m) (f a)

  -}
  flens
    :: (m ~ MappingsOf x, HasKeyAt k m r a)
    => BlueKey k
    -> Lens' (record f x) (f a)

instance HasFLens (Map :: (u -> Type) -> [Mapping Symbol u] -> Type) where
  flens (p :: BlueKey (k :: Symbol)) = findl p

instance HasFLens Rec' where
  flens p = _Rec . flens p

{-|

> vlens :: (HasKeyAt k m r a) => BlueKey k -> Lens' (Record m) a

-}
vlens
  :: ( HasFLens record
     , HasKeyAt k (MappingsOf x) r a)
  => BlueKey k -> Lens' (record Identity x) a
vlens p = flens p . _Identity

--------------------------------------------------------------------------------
--  Lenses for newtypes
--------------------------------------------------------------------------------

_Rec :: Lens (Rec' f m) (Rec' g m) (Map f m) (Map g m)
_Rec f (Rec s) = fmap Rec (f s)

_Identity :: Lens (Identity a) (Identity b) a b
_Identity f (Identity x) = fmap Identity (f x)
