{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Blueprint.Lens
  (
    -- * Lenses for tables
    clens
  , vlens
  , HasFLens(..)
    -- * Lenses for maps
  , HasKeyAt(..)
  , MapFind
  ) where

import Data.Kind (Type)
import GHC.TypeLits (Symbol)

import           Control.Lens.Lens

import           Blueprint.AsColumn
import           Blueprint.Internal.Schema
import           Blueprint.Internal.Map
import           Blueprint.Records
import           Blueprint.Labels

import           Typemap
import           Typemap.Lens


--------------------------------------------------------------------------------
--  Lenses for tables
--------------------------------------------------------------------------------

class HasFLens (record :: (u -> Type) -> t -> Type) where
  flens
    :: (m ~ MappingsOf x, HasKeyAt k m r a)
    => ColKey k
    -> Lens' (record f x) (f a)

instance HasFLens (Map :: (u -> Type) -> [Mapping Symbol u] -> Type) where
  flens (p :: ColKey (k :: Symbol)) = findl p

instance HasFLens Rec' where
  flens p = recLens . flens p

instance HasFLens TRec' where
  flens p = trecLens . flens p

vlens
  :: ( HasFLens record
     , HasKeyAt k (MappingsOf x) r a)
  => ColKey k -> Lens' (record Identity x) a
vlens p = flens p . _Identity

clens
  :: ( table ~ 'SchemaTable tname cols
     , HasKeyAt k cols r a)
  => ColKey k
  -> Lens' (TRec' (OverSql f) table) (f (SqlType a))
clens p = flens p . overSqlLens

--------------------------------------------------------------------------------
--  Lenses for newtypes
--------------------------------------------------------------------------------

recLens :: Lens (Rec' f m) (Rec' g m) (Map f m) (Map g m)
recLens f (Rec s) = fmap Rec (f s)

trecLens
  :: (MappingsOf table ~ cols)
  => Lens (TRec' f table) (TRec' g table) (Rec' f cols) (Rec' g cols)
trecLens f (TRec s) = fmap TRec (f s)

overSqlLens
  :: Lens (OverSql f a) (OverSql g a) (f (SqlType a)) (g (SqlType a))
overSqlLens f (OverSql x) = fmap OverSql (f x)

_Identity :: Lens (Identity a) (Identity b) a b
_Identity f (Identity x) = fmap Identity (f x)
