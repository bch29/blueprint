{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Blueprint.Internal.Map where

import           Data.Coerce
import           Data.Kind                       (Constraint)
import           GHC.TypeLits                    (Symbol)

import Data.Functor.Const (Const(..))

import           Data.Profunctor                 (Profunctor (..))
import           Data.Profunctor.Product
import           Data.Profunctor.Product.Default

import           Blueprint.Internal.Schema

import           Data.Singletons.Decide          ((:~:) (..), Decision (..),
                                                  SDecide (..))
import           Data.Singletons.Prelude.List    (Sing (..))

import           Typemap
import qualified Typemap.TypeLevel as Map
import qualified Typemap.Combinators as Map
import qualified Typemap.Singletons as Map

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
  (SCons (Map.SMapping sk) as') ->
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

mhead :: Map f ((k :-> a) ': m) -> f a
mhead (Ext _ v _) = v

mtail :: Map f (x ': m) -> Map f m
mtail (Ext _ _ s) = s

type family AllConstrained2Mapping f g c as :: Constraint where
  AllConstrained2Mapping f g c '[] = ()
  AllConstrained2Mapping f g c ((k :-> a) ': as) =
    (c (f a) (g a), AllConstrained2Mapping f g c as)
