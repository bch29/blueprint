{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE EmptyCase      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE PolyKinds      #-}
{-# LANGUAGE TypeOperators  #-}

module Blueprint.Internal.Vinyl where

import           Data.Vinyl

import           Data.Profunctor
import           Data.Profunctor.Product

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
      dimap (\(y :& _) -> y) (:&) x ****
      lmap (\(_ :& ys) -> ys) (pRecSing as' bs' xs)
  (SCons _ _, SNil) -> \case -- absurd
  (SNil, SCons _ _) -> \case -- absurd


--------------------------------------------------------------------------------
--  Util
--------------------------------------------------------------------------------

data Rec2 (f :: k1 -> k2 -> *) as bs where
  R2Nil :: Rec2 f '[] '[]
  R2Cons :: f a b -> Rec2 f as bs -> Rec2 f (a ': as) (b ': bs)

newtype Procompose f g p a b = Procompose { unProcompose :: p (f a) (g b) }
