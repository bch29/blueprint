{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Blueprint.Opaleye.Schema.Core where

import           Data.Kind                (Type)

import qualified Data.Text as Text

import           Data.Singletons
import           Data.Singletons.TypeLits (Symbol)

import           Blueprint.Core
import           Blueprint.Classes
import           Blueprint.Internal.Map

data BpkTable = BPTable Symbol

type T = 'BPTable

-- | The kind of SQL table blueprints.
type TableBlueprint = Blueprint BpkTable Type


-- | The kind of SQL schemas.
data SchemaBlueprint
  = SchemaBP Symbol [TableBlueprint]

--------------------------------------------------------------------------------
--  Instances
--------------------------------------------------------------------------------

data instance Sing (tn :: BpkTable) where
  SBpkTable :: Sing name -> Sing (T name)

data instance Sing (schema :: SchemaBlueprint) where
  SSchemaBP :: Sing name -> Sing tables -> Sing ('SchemaBP name tables)

instance (SingI name) => SingI (T name) where
  sing = SBpkTable sing

instance (SingI name, SingI tables) => SingI ('SchemaBP name tables) where
  sing = SSchemaBP sing sing


instance FormatShowBlueprint Identity BpkTable where
  formatShowRecord p =
    case singByProxy p of
      SBpkTable tname ->
        (("record <row from table '" ++ Text.unpack (fromSing tname) ++ "'>") ++) .
        concatMap (\(k, Some (Identity v)) -> "\n& #" ++ k ++ " =: " ++ show v)
