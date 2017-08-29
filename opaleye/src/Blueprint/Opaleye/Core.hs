{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Blueprint.Opaleye.Core where

import qualified Data.Text                         as Text

import           Control.Lens                      hiding (Identity)

import           Data.Profunctor                   (Profunctor (..))
import           Data.Profunctor.Product.Default

import           Data.Singletons

import           Opaleye.Column
import           Opaleye.Table

import           Typemap.Lens                      (HasKeyAt (..))

import           Blueprint.Core
import           Blueprint.Labels
import           Blueprint.Lens
import           Blueprint.Record
import           Blueprint.Record.Profunctor

import           Blueprint.Opaleye.AsColumn
import           Blueprint.Opaleye.Schema.Core

--------------------------------------------------------------------------------
-- * Records over SQL types

type OverSqlOf' f table = Rec' (OverSql f) table
type OverSqlOf f table = OverSqlOf' f (Normalize table)

--------------------------------------------------------------------------------
-- * Functors over SQL types

newtype OverSql f a = OverSql { getOverSql :: f (SqlType a) }

instance {-# INCOHERENT #-}
  ( Profunctor p
  , Default p (f (SqlType a)) b
  ) => Default p (OverSql f a) b where

  def = dimap getOverSql id def

instance {-# OVERLAPPABLE #-}
  ( Profunctor p
  , Default p a (f (SqlType b))
  ) => Default p a (OverSql f b) where

  def = dimap id OverSql def

--------------------------------------------------------------------------------
-- * Some database-specific record types

type ColumnsOf table = OverSqlOf Column table
type ColumnsOf' table = OverSqlOf' Column table

type TableOf' table = Table (ColumnsOf' table) (ColumnsOf' table)
type TableOf table = Table (ColumnsOf table) (ColumnsOf table)

--------------------------------------------------------------------------------
-- * Constructing Opaleye Tables

blueprintTable :: forall (table :: TableBlueprint) tname m. (SingI table, table ~ (tname :@ m)) => TableOf table
blueprintTable = blueprintTableProxy (Proxy :: Proxy table)

blueprintTableProxy
  :: (SingI (table :: TableBlueprint), table ~ (tname :@ m))
  => proxy table -> TableOf table
blueprintTableProxy p =
  case singByProxy p of
    tsing@(SBpkTable tname :%@ colsSing) ->
      let symStr = Text.unpack . fromSing
          cols = withSingI colsSing $
                 purePPRec tsing $
                 const . dimap getOverSql OverSql . required . symStr
      in Table (symStr tname) cols


--------------------------------------------------------------------------------
-- * Lenses

clens
  :: ( table ~ (tname :@ cols)
     , HasKeyAt k cols r a)
  => BlueKey k
  -> Lens' (Rec' (OverSql f) table) (f (SqlType a))
clens p = flens p . overSqlLens

overSqlLens
  :: Lens (OverSql f a) (OverSql g a) (f (SqlType a)) (g (SqlType a))
overSqlLens f (OverSql x) = fmap OverSql (f x)
