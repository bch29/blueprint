{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Blueprint.Internal.Lens where

import           Control.Lens

import           Data.Vinyl

import           Opaleye.Column

import           Data.Singletons

import           Blueprint.AsColumn
import           Blueprint.Internal.Schema
import           Blueprint.Internal.Records
import           Blueprint.Labels


columns :: proxy cname -> Lens' (Columns (cname :@ ty)) (Column (SqlType ty))
columns _ = lens getColumns (const Columns)

values :: proxy cname -> Lens' (Values (cname :@ ty)) ty
values _ = lens getValues (const Values)

trec :: Lens' (TRec f ('SchemaTable name columns)) (Rec f columns)
trec = lens getTRec (const TRec)

type family ColumnType cname columns where
  ColumnType cname ((cname :@ ty) ': columns) = ty
  ColumnType cname (_ ': columns) = ColumnType cname columns

recCol
  :: ( col ~ (cname :@ cty)
     , RElem col columns i
     , cty ~ ColumnType cname columns
     )
  => proxy cname -> Lens' (Rec f columns) (f (cname :@ cty))
recCol _ = rlens Proxy

colGeneral
  :: ( table ~ 'SchemaTable tname columns
     , col ~ (cname :@ cty)
     , RElem col columns i
     , ColumnType cname columns ~ cty
     )
  => proxy cname
  -> Lens' (TRec f table) (f col)
colGeneral _ = trec . recCol Proxy

col
  :: ( table ~ 'SchemaTable tname columns
     , col ~ (cname :@ cty)
     , RElem col columns i
     , ColumnType cname columns ~ cty
     )
  => LabelProxy cname
  -> Lens' (ColumnsOf table) (Column (SqlType cty))
col proxy = colGeneral proxy . columns proxy

col'
  :: forall cname cty col table tname columns i.
     ( table ~ 'SchemaTable tname columns
     , col ~ (cname :@ cty)
     , RElem col columns i
     , ColumnType cname columns ~ cty
     )
  => Lens' (ColumnsOf table) (Column (SqlType cty))
col' = col (LabelProxy :: LabelProxy cname)

val
  :: ( table ~ 'SchemaTable tname columns
     , col ~ (cname :@ cty)
     , RElem col columns i
     , ColumnType cname columns ~ cty
     )
  => LabelProxy cname
  -> Lens' (RecordOf table) cty
val proxy = colGeneral proxy . values proxy

val'
  :: forall cname cty col table tname columns i.
     ( table ~ 'SchemaTable tname columns
     , col ~ (cname :@ cty)
     , RElem col columns i
     , ColumnType cname columns ~ cty
     )
  => Lens' (RecordOf table) cty
val' = val (LabelProxy :: LabelProxy cname)
