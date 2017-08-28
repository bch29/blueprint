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

module Blueprint.Records
  (
    -- * General records
    -- ** Records over functorial values
    Rec'(..)
  , Rec
  , (=*)
    -- ** Records over normal values
  , Record'
  , Record
  , (=:)
    -- ** Building records
  , record
  , (&)
  , normalizeRec

    -- * Table records
  , TRec'(..)
  , TRec
  , OverSqlOf'
  , OverSqlOf
  , RecordOf
  , RecordOf'
  , normalizeTRec

    -- ** Functors over SQL values
  , OverSql(..)

    -- ** Helpers
  , NormalizeTable

    -- ** Constructing profunctors from table records
  , makeTable
  ) where

import           Data.Function                   ((&))
import           Data.Kind                       (Type)
import qualified Data.Text                       as Text

import Data.Aeson

import           Data.Profunctor                 (Profunctor (..))
import           Data.Profunctor.Product
import           Data.Profunctor.Product.Default

import           Data.Singletons
import           Data.Singletons.Prelude.List    (Sing (..))
import           Data.Singletons.TypeLits        (Symbol)

import           Typemap
import           Typemap.Mapping
import qualified Typemap.Combinators             as Map
import qualified Typemap.Singletons              as Map
import qualified Typemap.TypeLevel               as Map

import           Blueprint.AsColumn
import           Blueprint.Internal.Map
import           Blueprint.Internal.Schema
import           Blueprint.Labels
import           Blueprint.Records.Aeson

--------------------------------------------------------------------------------
-- * General records

newtype Rec' f (m :: [Mapping Symbol u]) = Rec { getRecMap :: Map f m }

type Rec f m = Rec' f (Map.AsMap m)

type Record' = Rec' Identity
type Record m = Record' (Map.AsMap m)

record :: Rec f '[]
record = Rec Empty

normalizeRec :: Rec' f m -> Rec f m
normalizeRec (Rec s) = Rec (Map.asMap s)

infix 2 =*
infix 2 =:

{-|

Insert the given key/value pair into the record. Overwrites the key if it
already exists.

> (=:) :: ColKey k -> f a -> Rec m -> Rec f (Insert (k :-> a) m)

-}
(=*) :: (SingI k)
     => ColKey k -> f a
     -> Rec' f m
     -> Rec' f (Map.Insert (k :-> a) m)
((_ :: ColKey k) =* x) (Rec s) =
  Rec (Map.mapInsert (sing :: Sing k) x s)

{-|

Insert the given key/value pair into the record. Overwrites the key if it
already exists.

> (=:) :: ColKey k -> a -> Record m -> Record (Insert (k :-> a) m)

-}
(=:) :: (SingI k)
     => ColKey k -> a
     -> Record' m
     -> Record' (Map.Insert (k :-> a) m)
k =: x = k =* Identity x

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

type family NormalizeTable table where
  NormalizeTable ('SchemaTable name cols) = 'SchemaTable name (Map.AsMap cols)

newtype TRec' (f :: Type -> Type) (table :: SchemaTable) = TRec
  { getTRec :: Rec' f (MappingsOf table) }

type TRec f table = TRec' f (NormalizeTable table)

normalizeTRec
  :: TRec' f ('SchemaTable name cols)
  -> TRec' f ('SchemaTable name (Map.AsMap cols))
normalizeTRec (TRec (Rec x)) = TRec (Rec (Map.asMap x))


type OverSqlOf' f table = TRec' (OverSql f) table
type OverSqlOf f table = OverSqlOf' f (NormalizeTable table)

type RecordOf' table = TRec' Identity table
type RecordOf table = RecordOf' (NormalizeTable table)


instance ( ProductProfunctor p
         , AllConstrained2Mapping f g (Default p) cols
         , table ~ 'SchemaTable name cols
         , SingI cols
         ) => Default p (TRec' f table) (TRec' g table) where

  def = dimap (getRecMap . getTRec) (TRec . Rec) (defMapSing sing)

--------------------------------------------------------------------------------
-- * Constructing profunctors from tables

makeTable
  :: forall table p p' f g. (ProductProfunctor p)

  => Sing table
  -> (forall a. String -> p (f a) (g a))
  -- ^ Lift each column into the profunctor @p@.

  -> (String -> p (TRec f table) (TRec g table)
             -> p' (TRec f table) (TRec g table))
  -- ^ Lift @p@ over table records to @p'@ over table records.

  -> p' (TRec f table) (TRec g table)
makeTable s f g = case s of
  SSchemaTable sName sCols ->
    g (Text.unpack $ fromSing sName)
      ( dimap (getRecMap . getTRec) (TRec . Rec)
      . pMap
      . makeColumns f
      $ Map.sAsMap sCols)

makeColumns
  :: (ProductProfunctor p)
  => (forall a. String -> p (f a) (g a))
  -> Sing (cols :: [SchemaColumn])
  -> Map (Procompose' f g p) cols
makeColumns f = \case
  SNil -> Empty
  SCons (SMapping sName _) sCols ->
    Ext sName (Procompose' (f (Text.unpack $ fromSing sName)))
    (makeColumns f sCols)

--------------------------------------------------------------------------------
-- * Other instances

instance (Map.ValsSatisfy ToJSON m) => ToJSON (Record' m) where
  toJSON (Rec s) = mapToJSON s

instance (Map.ValsSatisfy ToJSON (MappingsOf table)
         ) => ToJSON (RecordOf' table) where
  toJSON (TRec s) = toJSON s


instance (Map.ValsSatisfy FromJSON m
         , SingI m
         ) => FromJSON (Record' m) where
  parseJSON = fmap Rec . withObject "Record" (\o -> mapFromJSON o sing)


instance ( Map.ValsSatisfy FromJSON (MappingsOf table)
         , SingI table
         ) => FromJSON (RecordOf' table) where
  parseJSON =
    case sing :: Sing table of
      SSchemaTable sName sCols ->
        fmap (TRec . Rec) .
        withObject (Text.unpack $ fromSing sName)
                   (\o -> mapFromJSON o sCols)
