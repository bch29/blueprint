{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE EmptyCase      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE PolyKinds      #-}
{-# LANGUAGE TypeOperators  #-}

module Blueprint.Internal.Vinyl where

import Data.Kind (Constraint)

import           Data.Vinyl

import           Data.Profunctor
import           Data.Profunctor.Product
import           Data.Profunctor.Product.Default

import           Data.Singletons
import           Data.Singletons.Prelude.List

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


type family SatPair c ab :: Constraint where
  SatPair c '(a, b) = c a b

type family AllConstrained2 c as bs :: Constraint where
  AllConstrained2 c '[] '[] = ()
  AllConstrained2 c (a ': as) (b ': bs) = (c a b, AllConstrained2 c as bs)

type family AllConstrained2' f g c as bs :: Constraint where
  AllConstrained2' f g c '[] '[] = ()
  AllConstrained2' f g c (a ': as) (b ': bs) =
    (c (f a) (g b), AllConstrained2' f g c as bs)


defRecSing
  :: forall as bs p f g.
     ( ProductProfunctor p
     , AllConstrained2' f g (Default p) as bs)
  => Sing as -> Sing bs -> p (Rec f as) (Rec g bs)
defRecSing as bs = case (as, bs) of
  (SNil, SNil) -> purePP RNil
  (SCons (_ :: Sing a) (as' :: Sing as'), SCons (_ :: Sing b) (bs' :: Sing bs')) ->
    let x = def :: p (f a) (g b)
        xs = defRecSing as' bs' :: p (Rec f as') (Rec g bs')
    in dimap rhead (:&) x **** lmap rtail xs
  (SCons _ _, SNil) -> error "absurd"
  (SNil, SCons _ _) -> error "absurd"


-- instance
--   ( ProductProfunctor p
--   , AllConstrained2' f g (Default p) as bs
--   , SingI as, SingI bs
--   ) => Default p (Rec f as) (Rec g bs) where

--   def = defRecSing sing sing


--------------------------------------------------------------------------------
--  Util
--------------------------------------------------------------------------------

rhead :: Rec f (a ': as) -> f a
rhead (x :& _) = x

rtail :: Rec f (a ': as) -> Rec f as
rtail (_ :& xs) = xs

data Rec2 (f :: k1 -> k2 -> *) as bs where
  R2Nil :: Rec2 f '[] '[]
  R2Cons :: f a b -> Rec2 f as bs -> Rec2 f (a ': as) (b ': bs)

newtype Procompose f g p a b = Procompose { unProcompose :: p (f a) (g b) }
