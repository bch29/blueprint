{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE EmptyCase                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Blueprint.Internal.Vinyl where

import           Data.Kind                       (Constraint)
import           Data.Coerce

import           Data.Vinyl

import           Data.Profunctor
import           Data.Profunctor.Product
import           Data.Profunctor.Product.Default

import           Data.Singletons.Prelude.List (Sing(..))

import           Data.Type.Functor.Map                (Map (..), Mapping(..))
-- import qualified Data.Type.Functor.Map as Map
-- import qualified Data.Type.Set as Set

--------------------------------------------------------------------------------
--  Product profunctor adaptors for type maps
--------------------------------------------------------------------------------

pMap
  :: (ProductProfunctor p)
  => Map (Procompose' f g p) as
  -> p (Map f as) (Map g as)
pMap = \case
  Empty -> purePP Empty
  Ext k (Procompose' v) s ->
    dimap mhead (Ext k) v ****
    lmap mtail (pMap s)

mhead :: Map f ((k ':-> a) ': m) -> f a
mhead (Ext _ v _) = v

mtail :: Map f (x ': m) -> Map f m
mtail (Ext _ _ s) = s

--------------------------------------------------------------------------------
--  Product profunctor adaptors for Vinyl records
--------------------------------------------------------------------------------

pRec
  :: (ProductProfunctor p)
  => Rec (Procompose' f g p) as
  -> p (Rec f as) (Rec g as)
pRec = \case
    RNil -> purePP RNil

    Procompose' x :& xs ->
      dimap rhead (:&) x ****
      lmap rtail (pRec xs)

--------------------------------------------------------------------------------
--  Product profunctor default for Vinyl records
--------------------------------------------------------------------------------

defRecSing
  :: ( ProductProfunctor p
     , AllConstrained2' f g (Default p) as bs)
  => Sing as -> Sing bs -> p (Rec f as) (Rec g bs)
defRecSing as bs = case (as, bs) of
  (SNil, SNil) -> purePP RNil
  (SCons _ as', SCons _ bs') ->
    dimap rhead (:&) def **** lmap rtail (defRecSing as' bs')
  (SCons _ _, SNil) -> error "absurd"
  (SNil, SCons _ _) -> error "absurd"

--------------------------------------------------------------------------------
--  Pro/functor newtypes
--------------------------------------------------------------------------------

newtype Procompose f g p a b = Procompose { unProcompose :: p (f a) (g b) }
newtype Procompose' f g p a = Procompose' { getProcompose' :: p (f a) (g a) }

instance
  ( Default p (f a) (g b)
  ) => Default (Procompose f g p) a b where
  def = Procompose def

instance (Functor f, Functor g, Profunctor p)
  => Profunctor (Procompose f g p) where
  dimap (f :: a -> b) (g :: c -> d) =
    coerce (dimap (fmap f) (fmap g) :: p (f b) (g c) -> p (f a) (g d))


newtype Identity a = Identity { getIdentity :: a}
  deriving (Functor)

instance {-# INCOHERENT #-}
  ( Profunctor p , Default p a b
  ) => Default p (Identity a) b where
  def = dimap getIdentity id def

instance {-# OVERLAPPABLE #-}
  (Profunctor p, Default p a b
  ) => Default p a (Identity b) where
  def = dimap id Identity def

--------------------------------------------------------------------------------
--  Other
--------------------------------------------------------------------------------

rhead :: Rec f (a ': as) -> f a
rhead (x :& _) = x

rtail :: Rec f (a ': as) -> Rec f as
rtail (_ :& xs) = xs

type family SatPair c ab :: Constraint where
  SatPair c '(a, b) = c a b

type family AllConstrained2 c as bs :: Constraint where
  AllConstrained2 c '[] '[] = ()
  AllConstrained2 c (a ': as) (b ': bs) = (c a b, AllConstrained2 c as bs)

type family AllConstrained2' f g c as bs :: Constraint where
  AllConstrained2' f g c '[] '[] = ()
  AllConstrained2' f g c (a ': as) (b ': bs) =
    (c (f a) (g b), AllConstrained2' f g c as bs)
