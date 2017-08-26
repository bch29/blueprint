{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeInType #-}

module Blueprint.Internal.Schema where

import           Data.Kind                   (Type)

import           Data.Singletons.Prelude
import           Data.Singletons.TypeRepStar ()

-- | The kind of schema columns.
data SchemaColumn
  = SchemaColumn Symbol Type


-- | Shorthand for constructing a type of kind 'SchemaColumn'.
type (:@) = 'SchemaColumn


-- | The kind of schema tables.
data SchemaTable
  = SchemaTable Symbol [SchemaColumn]


-- | The kind of schemas.
data Schema
  = Schema Symbol [SchemaTable]


--------------------------------------------------------------------------------
--  Demoted Types
--------------------------------------------------------------------------------

data SchemaColumnRep = CDR (Demote Symbol) (Demote Type)
data SchemaTableRep = TDR (Demote Symbol) [Demote SchemaColumn]
data SchemaRep = SDR (Demote Symbol) [Demote SchemaTable]

--------------------------------------------------------------------------------
--  'Sing' instances
--------------------------------------------------------------------------------

data instance Sing (column :: SchemaColumn) where
  SSchemaColumn :: Sing name -> Sing ty -> Sing (name :@ ty)

data instance Sing (table :: SchemaTable) where
  SSchemaTable :: Sing name -> Sing cols -> Sing ('SchemaTable name cols)

data instance Sing (schema :: Schema) where
  SSchema :: Sing name -> Sing tables -> Sing ('Schema name tables)

instance SingKind SchemaColumn where
  type Demote SchemaColumn = SchemaColumnRep

  fromSing (SSchemaColumn name ty) = CDR (fromSing name) (fromSing ty)

  toSing (CDR name ty) =
    case (toSing name, toSing ty) of
      (SomeSing name', SomeSing ty') -> SomeSing (SSchemaColumn name' ty')

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


instance (SingI name, SingI ty) => SingI ('SchemaColumn name ty) where
  sing = SSchemaColumn sing sing

instance (SingI name, SingI cols) => SingI ('SchemaTable name cols) where
  sing = SSchemaTable sing sing

instance (SingI name, SingI tables) => SingI ('Schema name tables) where
  sing = SSchema sing sing
