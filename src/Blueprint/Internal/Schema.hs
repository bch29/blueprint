{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeInType    #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Blueprint.Internal.Schema where

import           Data.Kind                   (Type)

import           Data.Singletons.Prelude
import           Data.Singletons.TypeRepStar ()

import Data.Type.Map (Mapping(..))

-- | The kind of schema columns.
type SchemaColumn = Mapping Symbol Type


-- | Shorthand for constructing a type of kind 'SchemaColumn'.
type k :@ v = k ':-> v


-- | The kind of schema tables.
data SchemaTable
  = SchemaTable Symbol [SchemaColumn]


-- | The kind of schemas.
data Schema
  = Schema Symbol [SchemaTable]


--------------------------------------------------------------------------------
--  Demoted Types
--------------------------------------------------------------------------------

data MappingRep k v = MR (Demote k) (Demote v)
data SchemaTableRep = TDR (Demote Symbol) [Demote SchemaColumn]
data SchemaRep = SDR (Demote Symbol) [Demote SchemaTable]

--------------------------------------------------------------------------------
--  'Sing' instances
--------------------------------------------------------------------------------

data instance Sing (m :: Mapping k v) where
  SMapping :: Sing k -> Sing v -> Sing (k ':-> v)

data instance Sing (table :: SchemaTable) where
  SSchemaTable :: Sing name -> Sing cols -> Sing ('SchemaTable name cols)

data instance Sing (schema :: Schema) where
  SSchema :: Sing name -> Sing tables -> Sing ('Schema name tables)

instance (SingKind k, SingKind v) => SingKind (Mapping k v) where
  type Demote (Mapping k v) = MappingRep k v

  fromSing (SMapping name ty) = MR (fromSing name) (fromSing ty)

  toSing (MR k v) =
    case (toSing k, toSing v) of
      (SomeSing k', SomeSing v') -> SomeSing (SMapping k' v')

instance SingKind SchemaTable where
  type Demote SchemaTable = SchemaTableRep

  fromSing (SSchemaTable name cols) = TDR (fromSing name) (fromSing cols)

  toSing (TDR name cols) =
    case (toSing name, toSing cols) of
      (SomeSing name', SomeSing cols') -> SomeSing (SSchemaTable name' cols')

instance SingKind Schema where
  type Demote Schema = SchemaRep

  fromSing (SSchema name tables) = SDR (fromSing name) (fromSing tables)

  toSing (SDR name tables) =
    case (toSing name, toSing tables) of
      (SomeSing name', SomeSing tables') -> SomeSing (SSchema name' tables')


instance (SingI k, SingI v) => SingI (k ':-> v) where
  sing = SMapping sing sing

instance (SingI name, SingI cols) => SingI ('SchemaTable name cols) where
  sing = SSchemaTable sing sing

instance (SingI name, SingI tables) => SingI ('Schema name tables) where
  sing = SSchema sing sing
