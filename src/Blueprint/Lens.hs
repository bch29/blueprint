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

import           Control.Lens

import           Data.Vinyl

import           Data.Singletons

import           Blueprint.AsColumn
import           Blueprint.Internal.Schema
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
  -> Lens' (ColSqlOf f table) (f (SqlType cty))
col proxy = colGeneral proxy . colSql proxy

-- | Select an opaque column type from a table record using @TypeApplications@.
col'
  :: forall cname cty col f table tname cols i.
     ( table ~ 'SchemaTable tname cols
     , col ~ (cname :@ cty)
     , RElem col cols i
     , ColumnType cname cols ~ cty
     )
  => Lens' (ColSqlOf f table) (f (SqlType cty))
col' = col (ColumnAccessor :: ColumnAccessor cname)

val
  :: ( table ~ 'SchemaTable tname cols
     , col ~ (cname :@ cty)
     , RElem col cols i
     , ColumnType cname cols ~ cty
     )
  => ColumnAccessor cname
  -> Lens' (RecordOf table) cty
val proxy = colGeneral proxy . colVal proxy

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

recCol
  :: ( col ~ (cname :@ cty)
     , RElem col cols i
     , cty ~ ColumnType cname cols
     )
  => proxy cname -> Lens' (Rec f cols) (f (cname :@ cty))
recCol _ = rlens Proxy

colGeneral
  :: ( table ~ 'SchemaTable tname cols
     , col ~ (cname :@ cty)
     , RElem col cols i
     , ColumnType cname cols ~ cty
     )
  => proxy cname
  -> Lens' (TRec f table) (f col)
colGeneral _ = trec . recCol Proxy

colSql :: proxy cname -> Lens' (ColSql f (cname :@ ty)) (f (SqlType ty))
colSql _ = lens getColSql (const ColSql)

colVal :: proxy cname -> Lens' (ColVal (cname :@ ty)) ty
colVal _ = lens getColVal (const ColVal)

trec :: Lens' (TRec f ('SchemaTable name cols)) (Rec f cols)
trec = lens getTRec (const TRec)
