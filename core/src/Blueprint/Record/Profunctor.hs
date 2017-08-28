{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}

module Blueprint.Record.Profunctor where

import qualified Data.Text                    as Text

import           Data.Profunctor
import           Data.Profunctor.Product

import           Data.Singletons
import           Data.Singletons.Prelude.List (Sing (..))

import           Typemap
import           Typemap.Mapping
import qualified Typemap.Singletons           as Map

import           Blueprint.Internal.Map
import           Blueprint.Record

--------------------------------------------------------------------------------
-- * Constructing profunctors from tables

makeTable
  :: (ProductProfunctor p)

  => Sing (m :: Blueprint' u)
  -> (forall a. String -> p (f a) (g a))
  -- ^ Lift each column into the profunctor @p@.

  -> (p (Rec f m) (Rec g m) -> p' (Rec f m) (Rec g m))
  -- ^ Lift @p@ over table records to @p'@ over table records.

  -> p' (Rec f m) (Rec g m)
makeTable xs f g =
    g ( dimap getRecMap Rec
      . pMap
      . makeColumns f
      $ Map.sAsMap xs)

makeColumns
  :: (ProductProfunctor p)
  => (forall a. String -> p (f a) (g a))
  -> Sing (cols :: Blueprint' u)
  -> Map (Procompose' f g p) cols
makeColumns f = \case
  SNil -> Empty
  SCons (SMapping sName _) sCols ->
    Ext sName (Procompose' (f (Text.unpack $ fromSing sName)))
    (makeColumns f sCols)
