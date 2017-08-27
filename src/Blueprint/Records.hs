{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
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

--------------------------------------------------------------------------------
-- * Functors over SQL types

newtype OverSql f a = OverSql { getOverSql :: f (SqlType a) }

instance {-# INCOHERENT #-}
  ( Profunctor p
  , Default p (f (SqlType a)) b
  ) => Default p (OverSql f a) b where

  def = dimap getOverSql id def

instance {-# OVERLAPPABLE #-}
  ( Profunctor p
  , Default p a (f (SqlType b))
  ) => Default p a (OverSql f b) where

  def = dimap id OverSql def

--------------------------------------------------------------------------------
-- * Functors over types contained in column schemas

type family AppCol f col where
  AppCol f (_ :@ a) = f a

newtype OverCol f col = OverCol { getOverCol :: AppCol f col }

instance {-# INCOHERENT #-}
  ( col ~ (cname :@ a)
  , Profunctor p
  , Default p (f a) b
  ) => Default p (OverCol f col) b where

  def = dimap getOverCol id def

instance {-# OVERLAPPABLE #-}
  ( col ~ (cname :@ b)
  , Profunctor p
  , Default p a (f b)
  ) => Default p a (OverCol f col) where

  def = dimap id OverCol def

--------------------------------------------------------------------------------
-- * Table records

type family AppTable f table where
  AppTable f ('SchemaTable _ cols) = f cols

newtype TRec f table = TRec { getTRec :: AppTable (Rec (OverCol f)) table }


type OverSqlOf f table = TRec (OverSql f) table
type RecordOf table = TRec Identity table


instance ( ProductProfunctor p
         , AllConstrained2' (OverCol f) (OverCol g) (Default p) cols cols
         , table ~ 'SchemaTable name cols
         , SingI cols
         ) => Default p (TRec f table) (TRec g table) where

  def = dimap getTRec TRec (defRecSing sing sing)

--------------------------------------------------------------------------------
-- * Constructing profunctors from tables

makeTable
  :: forall table p p' f g. (SingI table, ProductProfunctor p)

  => (forall col. String -> p (f col) (g col))
  -- ^ Lift each column into the profunctor @p@.

  -> (String -> p (TRec f table) (TRec g table)
             -> p' (TRec f table) (TRec g table))
  -- ^ Lift @p@ over table records to @p'@ over table records.

  -> p' (TRec f table) (TRec g table)
makeTable f g = case (sing :: Sing table) of
  SSchemaTable sName sCols ->
    g (Text.unpack $ fromSing sName)
      ( dimap getTRec TRec
      . pRec
      . makeColumns (dimap getOverCol OverCol . f)
      $ sCols)

makeColumns
  :: (ProductProfunctor p)
  => (forall col cname cty. col ~ (cname :@ cty) => String -> p (f col) (g col))
  -> Sing (cols :: [SchemaColumn])
  -> Rec (Procompose' f g p) cols
makeColumns f = \case
  SNil -> RNil
  SCons (SSchemaColumn sName _) sCols ->
    Procompose' (f (Text.unpack $ fromSing sName))
    :& makeColumns f sCols
