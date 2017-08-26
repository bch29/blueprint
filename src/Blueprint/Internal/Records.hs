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

module Blueprint.Internal.Records where

import qualified Data.Text                    as Text

import           Data.Vinyl

import           Data.Profunctor
import           Opaleye.Column
import           Opaleye.Table

import           Data.Singletons
import           Data.Singletons.Prelude.List

import           Blueprint.AsColumn
import           Blueprint.Internal.Schema
import           Blueprint.Internal.Vinyl

--------------------------------------------------------------------------------
--  Columns
--------------------------------------------------------------------------------

data Columns (col :: SchemaColumn) where
  Columns :: Column (SqlType ty) -> Columns (name :@ ty)

getColumns :: Columns (name :@ ty) -> Column (SqlType ty)
getColumns = \case Columns x -> x

data Values (col :: SchemaColumn) where
  Values :: ty -> Values (name :@ ty)

--------------------------------------------------------------------------------
--  Tables
--------------------------------------------------------------------------------

data TRec f table where
  TRec :: Rec f columns -> TRec f ('SchemaTable name columns)

getTRec :: TRec f ('SchemaTable name columns) -> Rec f columns
getTRec = \case TRec x -> x

type ColumnsOf table = TRec Columns table
type RecordOf table = TRec Values table

table' :: SingI table => Table (ColumnsOf table) (ColumnsOf table)
table' = tableForSing sing

tableFor
  :: SingI table
  => proxy table -> Table (ColumnsOf table) (ColumnsOf table)
tableFor = tableForSing . singByProxy


tableForSing
  :: Sing (table :: SchemaTable)
  -> Table (ColumnsOf table) (ColumnsOf table)
tableForSing (SSchemaTable sName sCols) =
  Table
  (Text.unpack $ fromSing sName)
  (dimap getTRec TRec $ pRecSing sCols sCols (columnsProperties sCols))


columnsProperties
  :: Sing (cols :: [SchemaColumn])
  -> Rec2 (Procompose Columns Columns TableProperties) cols cols
columnsProperties = \case
  SNil -> R2Nil
  SCons (SSchemaColumn sName _) sCols ->
    Procompose (dimap getColumns Columns $
                required (Text.unpack $ fromSing sName))
    `R2Cons` columnsProperties sCols
