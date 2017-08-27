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

import           Data.Singletons
import           Data.Singletons.Prelude.List

--------------------------------------------------------------------------------
--  Product profunctor adaptors for Vinyl records
--------------------------------------------------------------------------------

pRec
  :: (ProductProfunctor p, SingI as, SingI bs)
  => Rec2 (Procompose f g p) as bs
  -> p (Rec f as) (Rec g bs)
pRec = pRecSing sing sing

pRecSing
  :: (ProductProfunctor p)
  => Sing as
  -> Sing bs
  -> Rec2 (Procompose f g p) as bs
  -> p (Rec f as) (Rec g bs)
pRecSing as bs = case (as, bs) of
  (SNil, SNil) -> \case
    R2Nil -> purePP RNil

  (SCons _ as', SCons _ bs') -> \case
    Procompose x `R2Cons` xs ->
      dimap rhead (:&) x ****
      lmap rtail (pRecSing as' bs' xs)
  (SCons _ _, SNil) -> \case -- absurd
  (SNil, SCons _ _) -> \case -- absurd

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

data Rec2 (f :: k1 -> k2 -> *) as bs where
  R2Nil :: Rec2 f '[] '[]
  R2Cons :: f a b -> Rec2 f as bs -> Rec2 f (a ': as) (b ': bs)

type family SatPair c ab :: Constraint where
  SatPair c '(a, b) = c a b

type family AllConstrained2 c as bs :: Constraint where
  AllConstrained2 c '[] '[] = ()
  AllConstrained2 c (a ': as) (b ': bs) = (c a b, AllConstrained2 c as bs)

type family AllConstrained2' f g c as bs :: Constraint where
  AllConstrained2' f g c '[] '[] = ()
  AllConstrained2' f g c (a ': as) (b ': bs) =
    (c (f a) (g b), AllConstrained2' f g c as bs)
