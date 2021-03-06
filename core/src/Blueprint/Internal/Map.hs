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

import           Data.Kind                       (Constraint)

import           Data.Profunctor                 (Profunctor (..))
import           Data.Profunctor.Product
import           Data.Profunctor.Product.Default

import           Data.Singletons.Prelude.List    (Sing (..))

import           Typemap
import           Typemap.Mapping

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

-- | We have to define a new identity functor because the wrong instance is
-- already defined for 'Default' over the standard identity functor.
--
-- TODO: Consider submitting a pull request in product-profunctors to fix this.
newtype Identity a = Identity { getIdentity :: a }
  deriving (Functor, Eq, Ord)

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
