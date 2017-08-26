{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators  #-}

module Types where

import           WebPrelude

import           Data.Int
import           Data.Word

import           Data.Singletons.TypeLits
import           Data.Vinyl
import           GHC.TypeLits

--------------------------------------------------------------------------------
--  Existentials
--------------------------------------------------------------------------------

data Some f where
  Some :: f a -> Some f

--------------------------------------------------------------------------------
--  Vectors
--------------------------------------------------------------------------------

data Vec a n where
  VNil :: Vec a 0
  VCons :: a -> Vec a n -> Vec a (1 + n)

data VarVec a n where
  VNil' :: VarVec a n
  VCons' :: a -> VarVec a n -> VarVec a (1 + n)

--------------------------------------------------------------------------------
--  Semantic Types
--------------------------------------------------------------------------------

type Bitvec = Vec Bool
type VarBitvec = VarVec Bool
type Charvec = Vec Char
type VarCharvec = VarVec Char

--------------------------------------------------------------------------------
--  Postgres Types
--------------------------------------------------------------------------------

data PgType a where

  -- Booleans --

  PTBoolean :: PgType Bool -- ^ boolean

  -- Integers --

  PTSmallint :: PgType Int16 -- ^ smallint
  PTInteger  :: PgType Int32 -- ^ integer
  PTBigint   :: PgType Int64 -- ^ bigint

  -- Serial --

  -- smallserial::serial2::autoincrementing two-byte integer
  -- serial::serial4::autoincrementing four-byte integer
  -- bigserial -- autoincrementing 8-byte int

  -- Floats --

  PTReal             :: PgType Float -- ^ real
  PTDoublePrecision :: PgType Double -- ^ double precision

  -- Other numbers --

  -- money              :: currency amount
  -- numeric [ (p, s) ] :: exact (decimal) numeric of selectable precision

  -- Sized vectors --

  PTBit
    :: SNat n -> PgType (Bitvec n) -- ^ bit ( n )

  PTBitVarying
    :: SNat n -> PgType (VarBitvec n) -- ^ bit varying ( n )

  PTCharacter
    :: SNat n
    -> PgType (Charvec n) -- ^ character ( n )

  PTCharacterVarying
    :: SNat n
    -> PgType (VarCharvec n) -- ^ character varying ( n )

  -- Unsized vectors

  PTBytea :: PgType ByteString -- ^ bytea
  PTText  :: PgType Text -- ^ text

  -- json  :: textual JSON data
  -- jsonb :: binary JSON data, decomposed
  -- xml :: XML data
  -- tsquery :: text search query
  -- tsvector :: text search document

  -- Shapes on a plane --

  -- box     :: rectangle on a plane
  -- circle  :: circle on a plane
  -- line    :: infinite line on a plane
  -- lseg    :: line segment on a plane
  -- path    :: geometric path on a plane
  -- point   :: geometric point on a plane
  -- polygon :: closed geometric path on a plane

  -- Networking --

  -- cidr :: IPv4 or IPv6 network address
  -- inet -- IPv4 or IPv6 host address
  -- macaddr :: MAC (Media Access Control) address

  -- Times and dates --

  -- date                                    :: calendar date (year, month, day)
  -- time [ (p) ] [ without time zone ]      :: time of day (no time zone)
  -- time [ (p) ] with time zone             :: time of day, including time zone
  -- timestamp [ (p) ] [ without time zone ] :: date and time (no time zone)
  -- timestamp [ (p) ] with time zone        :: date and time, including time zone
  -- interval [ fields ] [ (p) ]             :: time span

  -- Misc --

  -- pg_lsn :: PostgreSQL Log Sequence Number
  -- txid_snapshot :: user-level transaction ID snapshot

  PTUuid :: PgType UUID -- ^ uuid

--------------------------------------------------------------------------------
--  Constraints
--------------------------------------------------------------------------------

data TableRef

data PgConstraint where
  PCUnique :: PgConstraint
  PCNotNull :: PgConstraint
  PCPrimaryKey :: PgConstraint
  PCReferences :: Sing (tref :: TableRef) -> PgConstraint

--------------------------------------------------------------------------------
--  Type Specs
--------------------------------------------------------------------------------

data PgTypeSpec a where
  TSSimple
    :: PgType a -> PgTypeSpec a
  TSConstrained
    :: PgType a -> PgConstraint -> PgTypeSpec a


--------------------------------------------------------------------------------
--  Table Types
--------------------------------------------------------------------------------

data PgColumn f a where
  PgColumn :: SSymbol name -> PgType a -> f a -> PgColumn f a

data PgTable f as where
  PgTable :: SSymbol name -> Rec (PgColumn f) as -> PgTable f as
