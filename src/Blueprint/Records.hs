{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}

module Types where

import           WebPrelude

import           Data.Int

import           Data.Singletons.TypeLits
-- import           Data.Vinyl
import           GHC.TypeLits

import Schema

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
--  Haskell types for PostgreSQL types
--------------------------------------------------------------------------------

-- | Injective mapping of PostgreSQL types to the corresponding Haskell types.
type family PgTypeOf (pgty :: PgType) = (ty :: *) | ty -> pgty where
  PgTypeOf 'PgBoolean         = Bool
  PgTypeOf 'PgSmallint        = Int16
  PgTypeOf 'PgInteger         = Int32
  PgTypeOf 'PgBigint          = Int64
  PgTypeOf 'PgReal            = Float
  PgTypeOf 'PgDoublePrecision = Double

  -- Serial numbers --

  -- PgTypeOf 'PgSmallserial = _
  -- PgTypeOf 'PgSerial      = _
  -- PgTypeOf 'PgBigserial   = _

  -- Other numbers --

  -- PgTypeOf 'PgMoney  = _
  -- PgTypeOf ('PgNumeric p s) = _

  -- Sized vectors --

  -- PgTypeOf ('PgBit n)              = _
  -- PgTypeOf ('PgBitVarying n)       = _
  -- PgTypeOf ('PgCharacter n)        = _
  -- PgTypeOf ('PgCharacterVarying n) = _

  -- Binary blobs --

  PgTypeOf 'PgBytea = ByteString
  PgTypeOf 'PgText  = Text

  -- PgTypeOf 'PgJson     = _
  -- PgTypeOf 'PgJsonb    = _
  -- PgTypeOf 'PgXml      = _
  -- PgTypeOf 'PgTsquery  = _
  -- PgTypeOf 'PgTsvector = _

  -- Shapes on a plane --

  -- PgTypeOf 'PgBox     = _
  -- PgTypeOf 'PgCircle  = _
  -- PgTypeOf 'PgLine    = _
  -- PgTypeOf 'PgLseg    = _
  -- PgTypeOf 'PgPath    = _
  -- PgTypeOf 'PgPoint   = _
  -- PgTypeOf 'PgPolygon = _

  -- Networking --

  -- PgTypeOf 'PgCidr    = _
  -- PgTypeOf 'PgInet    = _
  -- PgTypeOf 'PgMacaddr = _

  -- Times and dates --

  -- PgTypeOf 'PgDate                         = _
  -- PgTypeOf ('PgTimeWithoutTimeZone p)      = _
  -- PgTypeOf ('PgTimeWithTimeZone p)         = _
  -- PgTypeOf ('PgTimestampWithoutTimeZone p) = _
  -- PgTypeOf ('PgTimestampWithTimeZone p)    = _
  -- PgTypeOf ('PgInterval p)                 = _


  -- TODO: interval quantity units

  -- Misc --

  -- PgTypeOf 'PgPgLsn        = _
  -- PgTypeOf 'PgTxidSnapshot = _
  PgTypeOf 'PgUuid         = UUID
