{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Blueprint.Lens
  ( col
  , col'
  , val
  , val'
  , ColumnType
  ) where

import           Control.Lens.Lens

import           Data.Vinyl

import           Data.Singletons

import           Blueprint.AsColumn
import           Blueprint.Internal.Schema
import           Blueprint.Internal.Vinyl
import           Blueprint.Labels
import           Blueprint.Records

-- | Select an opaque column type from a table record using a label.
col
  :: ( table ~ 'SchemaTable tname cols
     , col ~ (cname :@ cty)
     , RElem col cols i
     , ColumnType cname cols ~ cty
     )
  => ColumnAccessor cname
  -> Lens' (OverSqlOf f table) (f (SqlType cty))
col proxy = trecCol proxy . overCol . overSql

-- | Select an opaque column type from a table record using @TypeApplications@.
col'
  :: forall cname cty col f table tname cols i.
     ( table ~ 'SchemaTable tname cols
     , col ~ (cname :@ cty)
     , RElem col cols i
     , ColumnType cname cols ~ cty
     )
  => Lens' (OverSqlOf f table) (f (SqlType cty))
col' = col (ColumnAccessor :: ColumnAccessor cname)

val
  :: ( table ~ 'SchemaTable tname cols
     , col ~ (cname :@ cty)
     , RElem col cols i
     , ColumnType cname cols ~ cty
     )
  => ColumnAccessor cname
  -> Lens' (RecordOf table) cty
val proxy = trecCol proxy . overCol . _Identity

val'
  :: forall cname cty col table tname cols i.
     ( table ~ 'SchemaTable tname cols
     , col ~ (cname :@ cty)
     , RElem col cols i
     , ColumnType cname cols ~ cty
     )
  => Lens' (RecordOf table) cty
val' = val (ColumnAccessor :: ColumnAccessor cname)

--------------------------------------------------------------------------------
--  Type families
--------------------------------------------------------------------------------

type family ColumnType cname cols where
  ColumnType cname ((cname :@ ty) ': cols) = ty
  ColumnType cname (_ ': cols) = ColumnType cname cols

--------------------------------------------------------------------------------
--  Internal
--------------------------------------------------------------------------------

_Identity :: Lens (Identity a) (Identity b) a b
_Identity = lens getIdentity (const Identity)

overCol :: Lens (OverCol f (cname :@ a)) (OverCol g (cname :@ b)) (f a) (g b)
overCol = lens getOverCol (const OverCol)

overSql :: Lens (OverSql f a) (OverSql g b) (f (SqlType a)) (g (SqlType b))
overSql = lens getOverSql (const OverSql)

trec :: Lens' (TRec f ('SchemaTable name cols)) (Rec (OverCol f) cols)
trec = lens getTRec (const TRec)

recCol
  :: ( col ~ (cname :@ cty)
     , RElem col cols i
     , cty ~ ColumnType cname cols
     )
  => proxy cname -> Lens' (Rec f cols) (f (cname :@ cty))
recCol _ = rlens Proxy

trecCol
  :: ( table ~ 'SchemaTable tname cols
     , col ~ (cname :@ cty)
     , RElem col cols i
     , ColumnType cname cols ~ cty
     )
  => proxy cname
  -> Lens' (TRec f table) (OverCol f col)
trecCol _ = trec . recCol Proxy
