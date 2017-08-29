{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeInType    #-}
{-# LANGUAGE TypeOperators #-}

module Blueprint.Opaleye.Internal.Schema where

import           Data.Singletons
import           Data.Singletons.TypeLits (Symbol)

import           Blueprint.Record
import           Blueprint.Internal.Map


-- | The kind of schema tables.
data TableBlueprint
  = TableBP Symbol Blueprint


-- | The kind of schemas.
data SchemaBlueprint
  = SchemaBP Symbol [TableBlueprint]

--------------------------------------------------------------------------------
--  'Sing' instances
--------------------------------------------------------------------------------

data instance Sing (table :: TableBlueprint) where
  STableBP :: Sing name -> Sing cols -> Sing ('TableBP name cols)

data instance Sing (schema :: SchemaBlueprint) where
  SSchemaBP :: Sing name -> Sing tables -> Sing ('SchemaBP name tables)

instance (SingI name, SingI cols) => SingI ('TableBP name cols) where
  sing = STableBP sing sing

instance (SingI name, SingI tables) => SingI ('SchemaBP name tables) where
  sing = SSchemaBP sing sing

--------------------------------------------------------------------------------
--  Other instances
--------------------------------------------------------------------------------

type instance MappingsOf ('TableBP _ cols) = cols
