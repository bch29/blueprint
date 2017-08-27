{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Blueprint.Records where

import qualified Data.Text                       as Text

import           Data.Vinyl

import           Data.Profunctor
import           Data.Profunctor.Product
import           Data.Profunctor.Product.Default

import           Data.Singletons
import           Data.Singletons.Prelude.List

import           Blueprint.AsColumn
import           Blueprint.Internal.Schema
import           Blueprint.Internal.Vinyl
import           Blueprint.Labels

--------------------------------------------------------------------------------
-- * Column records

data ColSql f (col :: SchemaColumn) where
  ColSql :: f (SqlType ty) -> ColSql f (name :@ ty)

getColSql :: ColSql f (name :@ ty) -> f (SqlType ty)
getColSql = \case ColSql x -> x

data ColVal (col :: SchemaColumn) where
  ColVal :: ty -> ColVal (name :@ ty)

getColVal :: ColVal (name :@ ty) -> ty
getColVal = \case ColVal x -> x

instance ( col1 ~ (name1 :@ ty1)
         , col2 ~ (name2 :@ ty2)
         , Profunctor p
         , Default p (f (SqlType ty1)) (g (SqlType ty2))
         ) =>
         Default p (ColSql f col1) (ColSql g col2) where
  def = dimap getColSql ColSql def

instance ( col1 ~ (name1 :@ ty1)
         , col2 ~ (name2 :@ ty2)
         , Profunctor p
         , Default p (f (SqlType ty1)) ty2
         ) => Default p (ColSql f col1) (ColVal col2) where
  def = dimap getColSql ColVal def

instance ( col1 ~ (name1 :@ ty1)
         , col2 ~ (name2 :@ ty2)
         , Profunctor p
         , Default p ty1 (f (SqlType ty2))
         ) => Default p (ColVal col1) (ColSql f col2) where
  def = dimap getColVal ColSql def

--------------------------------------------------------------------------------
-- * Table records

data TRec f table where
  TRec :: Rec f colSql -> TRec f ('SchemaTable name colSql)

getTRec :: TRec f ('SchemaTable name colSql) -> Rec f colSql
getTRec = \case TRec x -> x

type ColSqlOf f table = TRec (ColSql f) table
type RecordOf table = TRec ColVal table

instance ( ProductProfunctor p
         , AllConstrained2' f g (Default p) colSql colSql
         , table ~ 'SchemaTable name colSql
         , SingI colSql
         ) => Default p (TRec f table) (TRec g table) where

  def = dimap getTRec TRec (defRecSing sing sing)

--------------------------------------------------------------------------------
-- * Constructing profunctors from tables

makeTable
  :: forall table p p' f g. (SingI table, ProductProfunctor p)

  => (forall col cname cty . col ~ (cname :@ cty)
      => String -> p (f col) (g col))
  -- ^ Lift each column into the profunctor @p@.

  -> (String -> p (TRec f table) (TRec g table)
             -> p' (TRec f table) (TRec g table))
  -- ^ Lift @p@ over table records to @p'@ over table records.

  -> p' (TRec f table) (TRec g table)
makeTable f g = case (sing :: Sing table) of
  SSchemaTable sName sCols ->
    g (Text.unpack $ fromSing sName)
      (dimap getTRec TRec $ pRecSing sCols sCols
       (makeColumns f sCols))

makeColumns
  :: (ProductProfunctor p)
  => (forall col cname cty. col ~ (cname :@ cty) => String -> p (f col) (g col))
  -> Sing (cols :: [SchemaColumn])
  -> Rec2 (Procompose f g p) cols cols
makeColumns f = \case
  SNil -> R2Nil
  SCons (SSchemaColumn sName _) sCols ->
    Procompose (f (Text.unpack $ fromSing sName))
    `R2Cons` makeColumns f sCols

--------------------------------------------------------------------------------
-- * Sugar for building table records

infixr 1 &:
infix 2 =:

(&:)
  :: f col -> TRec f ('SchemaTable name cols)
  -> TRec f ('SchemaTable name (col ': cols))
h &: (TRec t) = TRec (h :& t)

tnil :: TRec f ('SchemaTable name '[])
tnil = TRec RNil

(=:) :: ColumnAccessor cname -> ty -> ColVal (cname :@ ty)
_ =: x = ColVal x
