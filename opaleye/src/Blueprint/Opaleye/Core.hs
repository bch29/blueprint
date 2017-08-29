{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE UndecidableInstances  #-}

module Blueprint.Opaleye.Core where

import           Data.Kind                         (Type)

import qualified Data.Text                         as Text

import           Control.Lens                      hiding (Identity)

import           Data.Profunctor                   (Profunctor (..))
import           Data.Profunctor.Product
import           Data.Profunctor.Product.Default

import           Data.Singletons

import           Opaleye.Column
import           Opaleye.Table

import qualified Typemap.Combinators               as Map
import qualified Typemap.TypeLevel                 as Map

import           Blueprint.Internal.Map
import           Blueprint.Labels
import           Blueprint.Lens
import           Blueprint.Record
import           Blueprint.Record.Profunctor

import           Blueprint.Opaleye.AsColumn
import           Blueprint.Opaleye.Internal.Schema

--------------------------------------------------------------------------------
-- * Table records

type family NormalizeTable table where
  NormalizeTable ('TableBP name cols) = 'TableBP name (Map.AsMap cols)

newtype TRec' (f :: Type -> Type) (table :: TableBlueprint) = TRec { getTRec :: Rec' f (MappingsOf table) }

type TRec f table = TRec' f (NormalizeTable table)

normalizeTRec
  :: TRec' f ('TableBP name cols)
  -> TRec' f ('TableBP name (Map.AsMap cols))
normalizeTRec (TRec (Rec x)) = TRec (Rec (Map.asMap x))


type OverSqlOf' f table = TRec' (OverSql f) table
type OverSqlOf f table = OverSqlOf' f (NormalizeTable table)

type RecordOf' table = TRec' Identity table
type RecordOf table = RecordOf' (NormalizeTable table)


instance ( ProductProfunctor p
         , AllConstrained2Mapping f g (Default p) cols
         , table ~ 'TableBP name cols
         , SingI cols
         ) => Default p (TRec' f table) (TRec' g table) where

  def = dimap (getRecMap . getTRec) (TRec . Rec) (defMapSing sing)

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
-- * Constructing Opaleye Tables

type ColumnsOf table = OverSqlOf Column table
type ColumnsOf' table = OverSqlOf' Column table

type TableOf' table = Table (ColumnsOf' table) (ColumnsOf' table)
type TableOf table = Table (ColumnsOf table) (ColumnsOf table)

blueprintTable :: forall table. SingI table => TableOf table
blueprintTable = blueprintTableProxy (Proxy :: Proxy table)

blueprintTableProxy
  :: SingI table
  => proxy table -> TableOf table
blueprintTableProxy p =
  case singByProxy p of
    STableBP tname colsSing ->
      let symStr = Text.unpack . fromSing
          cols = withSingI colsSing $
                 purePPRec colsSing $
                 const . dimap getOverSql OverSql . required . symStr
          trec = dimap getTRec TRec cols
      in Table (symStr tname) trec


--------------------------------------------------------------------------------
-- * Lenses

instance HasFLens TRec' where
  flens p = _TRec . flens p

clens
  :: ( table ~ 'TableBP tname cols
     , HasKeyAt k cols r a)
  => BlueKey k
  -> Lens' (TRec' (OverSql f) table) (f (SqlType a))
clens p = flens p . overSqlLens

_TRec
  :: (MappingsOf table ~ cols)
  => Lens (TRec' f table) (TRec' g table) (Rec' f cols) (Rec' g cols)
_TRec f (TRec s) = fmap TRec (f s)

overSqlLens
  :: Lens (OverSql f a) (OverSql g a) (f (SqlType a)) (g (SqlType a))
overSqlLens f (OverSql x) = fmap OverSql (f x)
