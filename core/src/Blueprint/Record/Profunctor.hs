{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeInType    #-}
{-# LANGUAGE TypeOperators #-}

{-|

Profunctor combinators for blueprint records.

-}
module Blueprint.Record.Profunctor
  (
    purePPRec
  , purePPRec'
  , pRec
  , Procompose(..)
  ) where

import           Data.Profunctor              hiding (Mapping)
import           Data.Profunctor.Product

import           Data.Singletons
import           Data.Singletons.Prelude.List (Sing (..))
import           Data.Singletons.TypeLits     (Symbol)

import           Typemap
import           Typemap.Mapping
import qualified Typemap.Singletons           as Map

import           Blueprint.Internal.Map
import           Blueprint.Record
import           Blueprint.Core

{-|

This is a higher-order version of 'purePP' for 'ProductProfunctor'. Given a way
to construct a @p@ between values for each field, construct a @p@ between
records over the same fields.

Produces a profunctor between normalized records.

-}
purePPRec
  :: (b ~ (d :@ m), SingI m, ProductProfunctor p)
  => proxy (b :: Blueprint t u)
  -> (forall (k :: Symbol) (a :: u). Sing k -> Proxy a -> p (f a) (g a))
  -> p (Rec f b) (Rec g b)
purePPRec (_ :: proxy (d :@ m)) f =
  let sm = sing :: Sing m
  in withSingI (Map.sAsMap sm) (purePPRec' f)

{-|

This is a higher-order version of 'purePP' for 'ProductProfunctor'. Given a way
to construct a @p@ between values for each field, construct a @p@ between
records over the same fields.

-}
purePPRec'
  :: ((b :: Blueprint t u) ~ (d :@ m), SingI m, ProductProfunctor p)
  => (forall (k :: Symbol) (a :: u). Sing k -> Proxy a -> p (f a) (g a))
  -> p (Rec' f b) (Rec' g b)
purePPRec' f
  = dimap getRecMap Rec
  . pMap
  . purePPMap f
  $ sing

{-|

Product profunctor adaptor for records. Having a record over @'Procompose\'' f g
p@ is to say that for each field in the blueprint with value type @a@, we @p (f
a) (g a)@ in the record.

-}
pRec
  :: (b ~ (d :@ m), ProductProfunctor p)
  => Rec' (Procompose f g p) b
  -> p (Rec' f b) (Rec' g b)
pRec = dimap getRecMap Rec . pMap . getRecMap

--------------------------------------------------------------------------------
--  Types
--------------------------------------------------------------------------------

newtype Procompose f g p a = Procompose { getProcompose :: p (f a) (g a) }

-- TODO: Functor / Contravariant instances for 'Procompose'

--------------------------------------------------------------------------------
--  Combinators for maps
--------------------------------------------------------------------------------

purePPMap
  :: (ProductProfunctor p)
  => (forall (k1 :: k) (a :: u). Sing k1 -> Proxy a -> p (f a) (g a))
  -> Sing (m :: [Mapping k u])
  -> Map (Procompose f g p) m
purePPMap f = \case
  SNil -> Empty
  SCons (SMapping k v) sCols ->
    Ext k (Procompose (f k v))
    (purePPMap f sCols)

pMap
  :: (ProductProfunctor p)
  => Map (Procompose f g p) m
  -> p (Map f m) (Map g m)
pMap = \case
  Empty -> purePP Empty
  Ext k (Procompose v) s ->
    dimap mhead (Ext k) v ****
    lmap mtail (pMap s)
