{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Blueprint.Core where

import           Data.Kind                (Type)

import           Data.Singletons
import           Data.Singletons.TypeLits (Symbol)

import           Typemap
import qualified Typemap.TypeLevel        as Map

--------------------------------------------------------------------------------
--  Blueprints
--------------------------------------------------------------------------------

-- | The kind of blueprints. Members of this kind are types which act as
-- blueprints for records.
--
-- The two kind parameters are:
-- - @t@ is the kind of blueprint, which affects what can be built from it. For
--   example, 'BpkSimple' is a simple blueprint kind which can only be used for
--   forming records.
-- - @u@ is the 'universe' of values that the blueprint refers to. For example,
--   if @u ~ *@, then it refers to normal types.
--
-- A blueprint contains two type-level fields. The first is the attached data,
-- of kind @d@. The second is a list of key-value pairs, mapping field names to
-- the type contained in that field. Records formed from the blueprint will have
-- those fields.
data Blueprint t u = BP t [Mapping Symbol u]

-- | The simple blueprint kind, for normal records. See 'SimpleBP'.
data BpkSimple = BPSimple

-- | The single type of kind 'BpkSimple'.
type S = 'BPSimple

-- | A kind of simple blueprints with no attached data and normal type-level
-- values.
type SBlueprint = Blueprint BpkSimple Type

-- | Shorthand for constructing blueprints.
type (:@) = 'BP

-- | A simple blueprint with no attached data and the given key-value pairs.
type SimpleBP m = (S :@ m :: SBlueprint)

--------------------------------------------------------------------------------
--  Singletons
--------------------------------------------------------------------------------

data instance Sing (b :: Blueprint dkind u) where
  (:%@) :: Sing d -> Sing m -> Sing (d :@ m)

instance (SingI d, SingI m) => SingI (d :@ m) where
  sing = sing :%@ sing

data instance Sing (s :: BpkSimple) where
  SSimple :: Sing S

instance SingI S where
  sing = SSimple

--------------------------------------------------------------------------------
--  Type Families
--------------------------------------------------------------------------------

-- | Normalize the blueprint at the type level. Removes duplicate fields and
-- sorts fields in order of name. If there are multiple fields with the same
-- name and different value types, the first is used.
type family Normalize b where
  Normalize (d :@ m) = d :@ Map.AsMap m

-- | Get the key-value mapping contained in the blueprint.
type family MappingOf b where
  MappingOf (d :@ m) = m

-- | A constraint over all value types in the blueprint. Satisfied if every
-- value type satisfies the given unary constraint.
type family ValsSatisfy c b where
  ValsSatisfy c (d :@ m) = Map.ValsSatisfy c m

-- | Insert a new key-value pair into the blueprint. If it was originally in
-- normalized form, the result is in normalized form.
type family Insert kvp b where
  Insert kvp (d :@ m) = d :@ Map.Insert kvp m
