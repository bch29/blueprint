{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Blueprint.Record
  (
    -- * Blueprints
    Blueprint
  , Blueprint'
    -- * Records over normal values
  , Record'
  , Record
  , (=:)
    -- * Records over functorial values
  , Rec'(..)
  , Rec
  , (=*)
    -- * Building records
  , record
  , (&)
  , normalizeRec
  ) where

import           Data.Function                   ((&))
import           Data.Kind                       (Type)
import           Data.Monoid (All(..))

import           Data.Functor.Const

import           Data.Aeson

import           Data.Profunctor                 (Profunctor (..))
import           Data.Profunctor.Product
import           Data.Profunctor.Product.Default

import           Data.Singletons
import           Data.Singletons.Prelude.List    (Sing (..))
import           Data.Singletons.TypeLits        (Symbol)

import           Typemap
import qualified Typemap.Combinators             as Map
import qualified Typemap.TypeLevel               as Map

import           Blueprint.Aeson
import           Blueprint.Internal.Map
import           Blueprint.Labels

--------------------------------------------------------------------------------
-- Record Blueprints

type Blueprint' u = [Mapping Symbol u]
type Blueprint = [Mapping Symbol Type]

--------------------------------------------------------------------------------
-- General records

newtype Rec' f (m :: Blueprint' u) = Rec { getRecMap :: Map f m }

type Rec f m = Rec' f (Map.AsMap m)

type Record' = Rec' Identity
type Record (m :: Blueprint) = Record' (Map.AsMap m)

record :: Rec f '[]
record = Rec Empty

normalizeRec :: Rec' f m -> Rec f m
normalizeRec (Rec s) = Rec (Map.asMap s)

infix 2 =*
infix 2 =:

{-|

Insert the given key/value pair into the record. Overwrites the key if it
already exists.

> (=:) :: BlueKey k -> f a -> Rec m -> Rec f (Insert (k :-> a) m)

-}
(=*) :: (SingI k)
     => BlueKey k -> f a
     -> Rec' f m
     -> Rec' f (Map.Insert (k :-> a) m)
((_ :: BlueKey k) =* x) (Rec s) =
  Rec (Map.mapInsert (sing :: Sing k) x s)

{-|

Insert the given key/value pair into the record. Overwrites the key if it
already exists.

> (=:) :: BlueKey k -> a -> Record m -> Record (Insert (k :-> a) m)

-}
(=:) :: (SingI k)
     => BlueKey k -> a
     -> Record' m
     -> Record' (Map.Insert (k :-> a) m)
k =: x = k =* Identity x

--------------------------------------------------------------------------------
-- Instances

instance (Map.ValsSatisfy ToJSON m) => ToJSON (Record' m) where
  toJSON (Rec s) = mapToJSON s


instance (Map.ValsSatisfy FromJSON m
         , SingI m
         ) => FromJSON (Record' m) where
  parseJSON = fmap Rec . withObject "Record" (\o -> mapFromJSON o sing)


instance ( ProductProfunctor p
         , AllConstrained2Mapping f g (Default p) m
         , SingI m
         ) => Default p (Rec' f m) (Rec' g m) where

  def = dimap getRecMap Rec (defMapSing sing)


showFieldId
  :: Sing (k :: Symbol)
  -> Map.Constrained Show Identity a
  -> Const String a
showFieldId k (Map.Constrained (Identity v)) =
  Const $ "#" ++ show k ++ " := " ++ showsPrec 3 v ""

instance (Map.ValsSatisfy Show m) => Show (Record' m) where
  show = ("record " ++)
       . concatMap ("\n& " ++)
       . Map.toList
       . Map.mapWithKey showFieldId
       . Map.constrained
       . getRecMap


instance (Map.ValsSatisfy Eq m) => Eq (Record' m) where
  Rec s == Rec t =
    getAll $ Map.fold $
    Map.zipWith
    (\(Map.Constrained x) (Map.Constrained y) -> Const (All (x == y)))
    (Map.constrained @Eq s) (Map.constrained @Eq t)

-- | The 'Ord' instance might do unexpected things with normalized records
-- because it compares in order of fields, and normalization changes the order.
instance (Map.ValsSatisfy Eq m, Map.ValsSatisfy Ord m) => Ord (Record' m) where
  compare (Rec s) (Rec t) =
    Map.fold $
    Map.zipWith
    (\(Map.Constrained x) (Map.Constrained y) -> Const (compare x y))
    (Map.constrained @Ord s) (Map.constrained @Ord t)
