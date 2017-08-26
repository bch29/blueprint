{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Blueprint.Records where

import           WebPrelude

import           Data.Int

import           Data.Vinyl

import           Data.Singletons.Prelude
import           Data.Kind        (Type)

import           Opaleye.Column
import           Opaleye.PGTypes

import           Blueprint.Schema

--------------------------------------------------------------------------------
--  Classes
--------------------------------------------------------------------------------

-- class SqlType a where
--   type PgRepr a


--------------------------------------------------------------------------------
--  Records for tables
--------------------------------------------------------------------------------

data ColumnOf (col :: PgColumn) where
  ColumnOfSimple
    :: Column (ColumnTypeOf ty)
    -> ColumnOf (name :-? ty)

  ColumnOfConstrained
    :: Column (ColumnTypeOf ty)
    -> ColumnOf (name :-* (ty :-& constraints))

data ValueOf (col :: PgColumn) where
  ValueOfSimple
    :: ValueTypeOf ty
    -> ValueOf (name :-? ty)

  ValueOf
    :: ValueTypeOf ty
    -> ValueOf (name :-* (ty :-& constraints))


type family TableOf table where
  TableOf ('PgTable tname columns) = Rec ColumnOf columns

type family RecordOf table where
  RecordOf ('PgTable tname columns) = Rec ValueOf columns



--------------------------------------------------------------------------------
--  Haskell types for PostgreSQL types
--------------------------------------------------------------------------------

-- | Non-injective mapping of PostgreSQL types to the corresponding Haskell types.
type family ColumnTypeOf (pgty :: PgType) = (ty :: Type) where
  ColumnTypeOf 'PgBoolean         = PGBool
  ColumnTypeOf 'PgSmallint        = PGInt2
  ColumnTypeOf 'PgInteger         = PGInt4
  ColumnTypeOf 'PgBigint          = PGInt8
  ColumnTypeOf 'PgReal            = PGFloat4
  ColumnTypeOf 'PgDoublePrecision = PGFloat8

  -- Serial numbers --

  -- ColumnTypeOf 'PgSmallserial = _
  -- ColumnTypeOf 'PgSerial      = _
  -- ColumnTypeOf 'PgBigserial   = _

  -- Other numbers --

  -- ColumnTypeOf 'PgMoney  = _
  ColumnTypeOf ('PgNumeric p s) = PGNumeric

  -- Sized vectors --

  -- ColumnTypeOf ('PgBit n)              = _
  -- ColumnTypeOf ('PgBitVarying n)       = _
  -- ColumnTypeOf ('PgCharacter n)        = _
  -- ColumnTypeOf ('PgCharacterVarying n) = _

  -- Binary blobs --

  ColumnTypeOf 'PgBytea = PGBytea
  ColumnTypeOf 'PgText  = PGText

  ColumnTypeOf 'PgJson     = PGJson
  ColumnTypeOf 'PgJsonb    = PGJsonb
  -- ColumnTypeOf 'PgXml      = PGXml
  -- ColumnTypeOf 'PgTsquery  = _
  -- ColumnTypeOf 'PgTsvector = _

  -- Shapes on a plane --

  -- ColumnTypeOf 'PgBox     = _
  -- ColumnTypeOf 'PgCircle  = _
  -- ColumnTypeOf 'PgLine    = _
  -- ColumnTypeOf 'PgLseg    = _
  -- ColumnTypeOf 'PgPath    = _
  -- ColumnTypeOf 'PgPoint   = _
  -- ColumnTypeOf 'PgPolygon = _

  -- Networking --

  -- ColumnTypeOf 'PgCidr    = _
  -- ColumnTypeOf 'PgInet    = _
  -- ColumnTypeOf 'PgMacaddr = _

  -- Times and dates --

  ColumnTypeOf 'PgDate                         = PGDate
  ColumnTypeOf ('PgTimeWithoutTimeZone p)      = PGTime
  -- ColumnTypeOf ('PgTimeWithTimeZone p)         = PGTimestamptz
  ColumnTypeOf ('PgTimestampWithoutTimeZone p) = PGTimestamp
  ColumnTypeOf ('PgTimestampWithTimeZone p)    = PGTimestamptz
  -- ColumnTypeOf ('PgInterval p)                 = _


  -- TODO: interval quantity units

  -- Misc --

  -- ColumnTypeOf 'PgPgLsn        = _
  -- ColumnTypeOf 'PgTxidSnapshot = _
  ColumnTypeOf 'PgUuid         = PGUuid

-- | Injective mapping of PostgreSQL types to the corresponding Haskell types.
type family ValueTypeOf (pgty :: PgType) = (ty :: Type) | ty -> pgty where
  ValueTypeOf 'PgBoolean         = Bool
  ValueTypeOf 'PgSmallint        = Int16
  ValueTypeOf 'PgInteger         = Int32
  ValueTypeOf 'PgBigint          = Int64
  ValueTypeOf 'PgReal            = Float
  ValueTypeOf 'PgDoublePrecision = Double

  -- Serial numbers --

  -- ValueTypeOf 'PgSmallserial = _
  -- ValueTypeOf 'PgSerial      = _
  -- ValueTypeOf 'PgBigserial   = _

  -- Other numbers --

  -- ValueTypeOf 'PgMoney  = _
  -- ValueTypeOf ('PgNumeric p s) = _

  -- Sized vectors --

  -- ValueTypeOf ('PgBit n)              = _
  -- ValueTypeOf ('PgBitVarying n)       = _
  -- ValueTypeOf ('PgCharacter n)        = _
  -- ValueTypeOf ('PgCharacterVarying n) = _

  -- Binary blobs --

  ValueTypeOf 'PgBytea = ByteString
  ValueTypeOf 'PgText  = Text

  -- ValueTypeOf 'PgJson     = _
  -- ValueTypeOf 'PgJsonb    = _
  -- ValueTypeOf 'PgXml      = _
  -- ValueTypeOf 'PgTsquery  = _
  -- ValueTypeOf 'PgTsvector = _

  -- Shapes on a plane --

  -- ValueTypeOf 'PgBox     = _
  -- ValueTypeOf 'PgCircle  = _
  -- ValueTypeOf 'PgLine    = _
  -- ValueTypeOf 'PgLseg    = _
  -- ValueTypeOf 'PgPath    = _
  -- ValueTypeOf 'PgPoint   = _
  -- ValueTypeOf 'PgPolygon = _

  -- Networking --

  -- ValueTypeOf 'PgCidr    = _
  -- ValueTypeOf 'PgInet    = _
  -- ValueTypeOf 'PgMacaddr = _

  -- Times and dates --

  -- ValueTypeOf 'PgDate                         = _
  -- ValueTypeOf ('PgTimeWithoutTimeZone p)      = _
  -- ValueTypeOf ('PgTimeWithTimeZone p)         = _
  -- ValueTypeOf ('PgTimestampWithoutTimeZone p) = _
  -- ValueTypeOf ('PgTimestampWithTimeZone p)    = _
  -- ValueTypeOf ('PgInterval p)                 = _


  -- TODO: interval quantity units

  -- Misc --

  -- ValueTypeOf 'PgPgLsn        = _
  -- ValueTypeOf 'PgTxidSnapshot = _
  ValueTypeOf 'PgUuid         = UUID
