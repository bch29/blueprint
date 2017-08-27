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
  , tlens
    -- * Lenses for maps
  , AtKey(..)
  , MapFind
  , Found(..)
  , FoundType
  ) where

import           Control.Lens.Lens

import           Blueprint.AsColumn
import           Blueprint.Internal.Schema
import           Blueprint.Internal.Map
import           Blueprint.Records
import           Blueprint.Labels

import           Data.Type.Functor.Map


--------------------------------------------------------------------------------
--  Lenses for tables
--------------------------------------------------------------------------------

tlens
  :: ( table ~ 'SchemaTable tname cols
     , AtKey k cols r a)
  => ColKey k
  -> Lens' (TRec f table) (f a)
tlens p = trecLens . klens p

vlens
  :: ( table ~ 'SchemaTable tname cols
     , AtKey k cols r a)
  => ColKey k
  -> Lens' (RecordOf table) a
vlens p = tlens p . _Identity

clens
  :: ( table ~ 'SchemaTable tname cols
     , AtKey k cols r a)
  => ColKey k
  -> Lens' (OverSqlOf f table) (f (SqlType a))
clens p = tlens p . overSqlLens

--------------------------------------------------------------------------------
--  Lenses for newtypes
--------------------------------------------------------------------------------

trecLens
  :: (table ~ 'SchemaTable tname cols)
  => Lens (TRec f table) (TRec g table) (Map f cols) (Map g cols)
trecLens f (TRec s) = fmap TRec (f s)

overSqlLens
  :: Lens (OverSql f a) (OverSql g a) (f (SqlType a)) (g (SqlType a))
overSqlLens f (OverSql x) = fmap OverSql (f x)

_Identity :: Lens (Identity a) (Identity b) a b
_Identity f (Identity x) = fmap Identity (f x)

--------------------------------------------------------------------------------
--  Lenses for maps
--------------------------------------------------------------------------------

class (MapFind k m ~ r, FoundType r ~ a) => AtKey k m r a where
  klens :: proxy k -> Lens' (Map f m) (f a)

instance AtKey k ((k ':-> a) ': m) ('Here a) a where
  klens _ f (Ext k v s) = fmap (\v' -> Ext k v' s) (f v)

instance
  ( AtKey k m r a
  , MapFind k (kvp ': m) ~ ('There r)
  ) => AtKey k (kvp ': m) ('There r) a where
  klens p f (Ext k v s) = fmap (Ext k v) (klens p f s)

type family MapFind k m where
  MapFind k ((k ':-> a) ': m) = 'Here a
  MapFind k (_ ': m) = 'There (MapFind k m)

data Found v
  = Here v
  | There (Found v)

type family FoundType r where
  FoundType ('Here a) = a
  FoundType ('There r) = FoundType r
