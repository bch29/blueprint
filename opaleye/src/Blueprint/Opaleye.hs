module Blueprint.Opaleye where

import           Data.Profunctor                 (Profunctor (..))
import           Data.Profunctor.Product
import           Data.Profunctor.Product.Default

--------------------------------------------------------------------------------
-- * Table records

type family NormalizeTable table where
  NormalizeTable ('SchemaTable name cols) = 'SchemaTable name (Map.AsMap cols)

newtype TRec' (f :: Type -> Type) (table :: SchemaTable) = TRec
  { getTRec :: Rec' f (MappingsOf table) }

type TRec f table = TRec' f (NormalizeTable table)

normalizeTRec
  :: TRec' f ('SchemaTable name cols)
  -> TRec' f ('SchemaTable name (Map.AsMap cols))
normalizeTRec (TRec (Rec x)) = TRec (Rec (Map.asMap x))


type OverSqlOf' f table = TRec' (OverSql f) table
type OverSqlOf f table = OverSqlOf' f (NormalizeTable table)

type RecordOf' table = TRec' Identity table
type RecordOf table = RecordOf' (NormalizeTable table)


instance ( ProductProfunctor p
         , AllConstrained2Mapping f g (Default p) cols
         , table ~ 'SchemaTable name cols
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
--  Lenses
--------------------------------------------------------------------------------

instance HasFLens TRec' where
  flens p = trecLens . flens p

clens
  :: ( table ~ 'SchemaTable tname cols
     , HasKeyAt k cols r a)
  => ColKey k
  -> Lens' (TRec' (OverSql f) table) (f (SqlType a))
clens p = flens p . overSqlLens

trecLens
  :: (MappingsOf table ~ cols)
  => Lens (TRec' f table) (TRec' g table) (Rec' f cols) (Rec' g cols)
trecLens f (TRec s) = fmap TRec (f s)

overSqlLens
  :: Lens (OverSql f a) (OverSql g a) (f (SqlType a)) (g (SqlType a))
overSqlLens f (OverSql x) = fmap OverSql (f x)
