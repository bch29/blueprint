{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeInType    #-}
{-# LANGUAGE TypeOperators #-}

module Blueprint.Internal.Schema where

import           Data.Kind               (Type)

import           Data.Singletons.Prelude

import           Typemap

-- | The kind of schema columns.
type SchemaColumn = Mapping Symbol Type


-- | The kind of schema tables.
data SchemaTable
  = SchemaTable Symbol [SchemaColumn]


-- | The kind of schemas.
data Schema
  = Schema Symbol [SchemaTable]


--------------------------------------------------------------------------------
--  Demoted Types
--------------------------------------------------------------------------------

data SchemaTableRep = TDR (Demote Symbol) [Demote SchemaColumn]
data SchemaRep = SDR (Demote Symbol) [Demote SchemaTable]

--------------------------------------------------------------------------------
--  'Sing' instances
--------------------------------------------------------------------------------

data instance Sing (table :: SchemaTable) where
  SSchemaTable :: Sing name -> Sing cols -> Sing ('SchemaTable name cols)

data instance Sing (schema :: Schema) where
  SSchema :: Sing name -> Sing tables -> Sing ('Schema name tables)

instance (SingI name, SingI cols) => SingI ('SchemaTable name cols) where
  sing = SSchemaTable sing sing

instance (SingI name, SingI tables) => SingI ('Schema name tables) where
  sing = SSchema sing sing
