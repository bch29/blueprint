{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Blueprint.Internal.Records where

import qualified Data.Text                       as Text

import           Data.Vinyl

import           Data.Profunctor
import           Data.Profunctor.Product
import           Data.Profunctor.Product.Default

import           Opaleye.Column
import           Opaleye.Table

import           Data.Singletons
import           Data.Singletons.Prelude.List

import           Blueprint.AsColumn
import           Blueprint.Internal.Schema
import           Blueprint.Internal.Vinyl
import           Blueprint.Labels

--------------------------------------------------------------------------------
--  Columns
--------------------------------------------------------------------------------

data Columns (col :: SchemaColumn) where
  Columns :: Column (SqlType ty) -> Columns (name :@ ty)

getColumns :: Columns (name :@ ty) -> Column (SqlType ty)
getColumns = \case Columns x -> x

data Values (col :: SchemaColumn) where
  Values :: ty -> Values (name :@ ty)

getValues :: Values (name :@ ty) -> ty
getValues = \case Values x -> x

instance ( col1 ~ (name1 :@ ty1)
         , col2 ~ (name2 :@ ty2)
         , Profunctor p
         , Default p (Column (SqlType ty1)) (Column (SqlType ty2))
         ) =>
         Default p (Columns col1) (Columns col2) where
  def = dimap getColumns Columns def

instance ( col1 ~ (name1 :@ ty1)
         , col2 ~ (name2 :@ ty2)
         , Profunctor p
         , Default p (Column (SqlType ty1)) ty2
         ) => Default p (Columns col1) (Values col2) where
  def = dimap getColumns Values def

instance ( col1 ~ (name1 :@ ty1)
         , col2 ~ (name2 :@ ty2)
         , Profunctor p
         , Default p ty1 (Column (SqlType ty2))
         ) => Default p (Values col1) (Columns col2) where
  def = dimap getValues Columns def

--------------------------------------------------------------------------------
--  Tables
--------------------------------------------------------------------------------

data TRec f table where
  TRec :: Rec f columns -> TRec f ('SchemaTable name columns)

getTRec :: TRec f ('SchemaTable name columns) -> Rec f columns
getTRec = \case TRec x -> x

type ColumnsOf table = TRec Columns table
type RecordOf table = TRec Values table
type TableOf table = Table (ColumnsOf table) (ColumnsOf table)

instance ( ProductProfunctor p
         , AllConstrained2' f g (Default p) columns columns
         , table ~ 'SchemaTable name columns
         , SingI columns
         ) => Default p (TRec f table) (TRec g table) where

  def = dimap getTRec TRec (defRecSing sing sing)

infixr 1 &:
infix 2 =:

(&:)
  :: f col -> TRec f ('SchemaTable name cols)
  -> TRec f ('SchemaTable name (col ': cols))
h &: (TRec t) = TRec (h :& t)

tnil :: TRec f ('SchemaTable name '[])
tnil = TRec RNil

(=:) :: LabelProxy cname -> ty -> Values (cname :@ ty)
_ =: x = Values x

--------------------------------------------------------------------------------
--  Constructing Opaleye Tables
--------------------------------------------------------------------------------

table' :: SingI table => TableOf table
table' = tableForSing sing

tableFor
  :: SingI table
  => proxy table -> TableOf table
tableFor = tableForSing . singByProxy

--------------------------------------------------------------------------------
--  Internal
--------------------------------------------------------------------------------


tableForSing
  :: Sing (table :: SchemaTable)
  -> TableOf table
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
