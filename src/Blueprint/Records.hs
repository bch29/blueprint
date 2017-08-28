{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Blueprint.Records where

import qualified Data.Text                       as Text

import           Data.Profunctor                 (Profunctor (..))
import           Data.Profunctor.Product
import           Data.Profunctor.Product.Default

import           Data.Singletons
import           Data.Singletons.Prelude.List    (Sing (..))

import           Typemap
import qualified Typemap.TypeLevel as Map
import qualified Typemap.Combinators as Map
import qualified Typemap.Singletons as Map

import           Blueprint.AsColumn
import           Blueprint.Internal.Schema
import           Blueprint.Internal.Map

--------------------------------------------------------------------------------
-- * General records

newtype Record' f m = Record (Map f m)

type Record f m = Record' f (Map.AsMap m)

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
-- * Table records

type family AppTable f table where
  AppTable f ('SchemaTable _ cols) = f cols

newtype TRec f table = TRec { getTRec :: AppTable (Map f) table }


type OverSqlOf f table = TRec (OverSql f) table
type RecordOf table = TRec Identity table


instance ( ProductProfunctor p
         , AllConstrained2Mapping f g (Default p) cols
         , table ~ 'SchemaTable name cols
         , SingI cols
         ) => Default p (TRec f table) (TRec g table) where

  def = dimap getTRec TRec (defMapSing sing)

--------------------------------------------------------------------------------
-- * Constructing profunctors from tables

makeTable
  :: forall table p p' f g. (SingI table, ProductProfunctor p)

  => (forall a. String -> p (f a) (g a))
  -- ^ Lift each column into the profunctor @p@.

  -> (String -> p (TRec f table) (TRec g table)
             -> p' (TRec f table) (TRec g table))
  -- ^ Lift @p@ over table records to @p'@ over table records.

  -> p' (TRec f table) (TRec g table)
makeTable f g = case (sing :: Sing table) of
  SSchemaTable sName sCols ->
    g (Text.unpack $ fromSing sName)
      ( dimap getTRec TRec
      . pMap
      . makeColumns f
      $ sCols)

makeColumns
  :: (ProductProfunctor p)
  => (forall a. String -> p (f a) (g a))
  -> Sing (cols :: [SchemaColumn])
  -> Map (Procompose' f g p) cols
makeColumns f = \case
  SNil -> Empty
  SCons (Map.SMapping sName) sCols ->
    Ext sName (Procompose' (f (Text.unpack $ fromSing sName)))
    (makeColumns f sCols)
