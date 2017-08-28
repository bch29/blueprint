{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Blueprint.Internal.Map where

import           Data.Coerce
import           Data.Kind                       (Constraint)

import           Data.Profunctor                 (Profunctor (..))
import           Data.Profunctor.Product
import           Data.Profunctor.Product.Default

import           Data.Singletons.Prelude (Symbol)
import           Data.Singletons.Prelude.List    (Sing (..))

import           Typemap
import           Typemap.Mapping

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

--------------------------------------------------------------------------------
--  Product profunctor default for type maps
--------------------------------------------------------------------------------

defMapSing
  :: ( ProductProfunctor p
     , AllConstrained2Mapping f g (Default p) m)
  => Sing m -> p (Map f m) (Map g m)
defMapSing as = case as of
  SNil -> purePP Empty
  (SCons (SMapping sk _) as') ->
    dimap mhead (Ext sk) def **** lmap mtail (defMapSing as')

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


newtype Identity a = Identity { getIdentity :: a }
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

mhead :: Map f ((k :-> a) ': m) -> f a
mhead (Ext _ v _) = v

mtail :: Map f (x ': m) -> Map f m
mtail (Ext _ _ s) = s

type family AllConstrained2Mapping f g c as :: Constraint where
  AllConstrained2Mapping f g c '[] = ()
  AllConstrained2Mapping f g c ((k :-> a) ': as) =
    (c (f a) (g a), AllConstrained2Mapping f g c as)


type family MappingsOf (t :: x) :: [Mapping Symbol u]
type instance MappingsOf (m :: [Mapping Symbol u]) = m
