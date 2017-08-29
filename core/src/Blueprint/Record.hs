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
    -- * Records over normal values
    Record'
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
import           Data.Monoid (All(..))

import           Data.Functor.Const

import           Data.Aeson
import qualified Data.Text as Text

import           Data.Profunctor                 (Profunctor (..))
import           Data.Profunctor.Product
import           Data.Profunctor.Product.Default

import           Data.Singletons
import           Data.Singletons.Prelude.List    (Sing (..))
import           Data.Singletons.TypeLits        (Symbol)

import           Typemap
import qualified Typemap.Combinators             as Map

import           Blueprint.Core
import           Blueprint.Aeson
import           Blueprint.Internal.Map
import           Blueprint.Labels
import           Blueprint.Classes

--------------------------------------------------------------------------------
-- General records

data Rec' f b where
  Rec :: { getRecMap :: Map f m } -> Rec' f (d :@ m)

type Rec f b = Rec' f (Normalize b)

type Record' b = Rec' Identity b
type Record b = Record' (Normalize b)

record :: Rec f (d :@ '[])
record = Rec Empty

normalizeRec :: Rec' f b -> Rec f b
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
     -> Rec' f b
     -> Rec' f (Insert (k :-> a) b)
((_ :: BlueKey k) =* x) (Rec s) =
  Rec (Map.mapInsert (sing :: Sing k) x s)

{-|

Insert the given key/value pair into the record. Overwrites the key if it
already exists.

> (=:) :: BlueKey k -> a -> Record m -> Record (Insert (k :-> a) m)

-}
(=:) :: (SingI k)
     => BlueKey k -> a
     -> Record' b
     -> Record' (Insert (k :-> a) b)
k =: x = k =* Identity x

--------------------------------------------------------------------------------
-- Instances

instance (ValsSatisfy ToJSON b) => ToJSON (Record' b) where
  toJSON (Rec s) = mapToJSON s


instance ( b ~ (d :@ m)
         , ValsSatisfy FromJSON b
         , SingI m
         ) => FromJSON (Record' b) where
  parseJSON = fmap Rec . withObject "Record" (\o -> mapFromJSON o sing)


instance ( ProductProfunctor p
         , AllConstrained2Mapping f g (Default p) m
         , b ~ (d :@ m)
         , SingI m
         ) => Default p (Rec' f b) (Rec' g b) where

  def = dimap getRecMap Rec (defMapSing sing)


showFieldPair
  :: Sing (k :: Symbol)
  -> Map.Constrained Show f a
  -> Const (String, Some Show f) a
showFieldPair k (Map.Constrained v) =
  Const $ (Text.unpack (fromSing k), Some v)

instance ( b ~ ((d :: dkind) :@ m)
         , SingI d
         , FormatShowBlueprint f dkind
         , ValsSatisfy Show b
         ) => Show (Rec' f b) where
  show = formatShowRecord (Proxy :: Proxy d)
       . Map.toList
       . Map.mapWithKey showFieldPair
       . Map.constrained
       . getRecMap


instance (ValsSatisfy Eq b) => Eq (Record' b) where
  Rec s == Rec t =
    getAll $ Map.fold $
    Map.zipWith
    (\(Map.Constrained x) (Map.Constrained y) -> Const (All (x == y)))
    (Map.constrained @Eq s) (Map.constrained @Eq t)

-- | The 'Ord' instance might do unexpected things with normalized records
-- because it compares in order of fields, and normalization changes the order.
instance (ValsSatisfy Eq b, ValsSatisfy Ord b) => Ord (Record' b) where
  compare (Rec s) (Rec t) =
    Map.fold $
    Map.zipWith
    (\(Map.Constrained x) (Map.Constrained y) -> Const (compare x y))
    (Map.constrained @Ord s) (Map.constrained @Ord t)
