{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}

module Blueprint.Schema where

import           Data.Singletons.TypeLits
import           GHC.TypeLits

--------------------------------------------------------------------------------
--  Postgres Types
--------------------------------------------------------------------------------

type Precision = Nat
type Scale = Nat

data PgType =

  -- Basic types --

    PgBoolean         -- ^ Boolean values
  | PgSmallint        -- ^ 2-byte signed integers
  | PgInteger         -- ^ 4-byte signed integers
  | PgBigint          -- ^ 8-byte signed integers
  | PgReal            -- ^ 4-byte floating point numbers
  | PgDoublePrecision -- ^ 8-byte floating point numbers

  -- Serial numbers --

  | PgSmallserial -- ^ Auto-incrementing 2-byte integers
  | PgSerial      -- ^ Auto-incrementing 4-byte integers
  | PgBigserial   -- ^ Auto-incrementing 8-byte integers

  -- Other numbers --

  | PgMoney -- ^ Currency amount
  | PgNumeric Precision Scale
    -- ^ Arbitrary-precision decimal number with given number of digits total
    -- ('Precision') and number of digits after the decimal point ('Scale').

  -- Sized vectors --

  | PgBit Nat              -- ^ Fixed-length bit vector
  | PgBitVarying Nat       -- ^ Variable-length bit vector
  | PgCharacter Nat        -- ^ Fixed-length character vector
  | PgCharacterVarying Nat -- ^ Variable-length character vector

  -- Binary blobs --

  | PgBytea -- ^ Byte array
  | PgText  -- ^ Text blob

  | PgJson     -- ^ Textual JSON
  | PgJsonb    -- ^ Binary JSON
  | PgXml      -- ^ XML data
  | PgTsquery  -- ^ Text search query
  | PgTsvector -- ^ Text search document

  -- Shapes on a plane --

  | PgBox     -- ^ rectangle on a plane
  | PgCircle  -- ^ circle on a plane
  | PgLine    -- ^ infinite line on a plane
  | PgLseg    -- ^ line segment on a plane
  | PgPath    -- ^ geometric path on a plane
  | PgPoint   -- ^ geometric point on a plane
  | PgPolygon -- ^ closed geometric path on a plane

  -- Networking --

  | PgCidr    -- ^ IPv4 or IPv6 network address
  | PgInet    -- ^ IPv4 or IPv6 host address
  | PgMacaddr -- ^ MAC (Media Access Control) address

  -- Times and dates --

  | PgDate -- ^ calendar date (year, month, day)
  | PgTimeWithoutTimeZone (Maybe Precision)
    -- ^ time of day (no time zone)
  | PgTimeWithTimeZone (Maybe Precision)
    -- ^ time of day, including time zone
  | PgTimestampWithoutTimeZone (Maybe Precision)
    -- ^ date and time (no time zone)
  | PgTimestampWithTimeZone (Maybe Precision)
    -- ^ date and time, including time zone
  | PgInterval (Maybe Precision)
    -- ^ time span

  -- TODO: interval quantity units

  -- Misc --

  | PgPgLsn        -- ^ PostgreSQL Log Sequence Number
  | PgTxidSnapshot -- ^ user-level transaction ID snapshot
  | PgUuid         -- ^ Universal unique identifier

--------------------------------------------------------------------------------
--  Synonyms for common types
--------------------------------------------------------------------------------

type PgTimestamp = 'PgTimestampWithTimeZone 'Nothing
type PgTimestamp' = 'PgTimestampWithoutTimeZone 'Nothing

--------------------------------------------------------------------------------
--  Constraints
--------------------------------------------------------------------------------

data PgTableRef

data PgConstraint =
    PgUnique
  | PgNotNull
  | PgPrimaryKey
  | PgReferences PgTableRef

data PgTypeSpec
  = PgSimple PgType
  | PgConstrained PgType [PgConstraint]

--------------------------------------------------------------------------------
--  Tables
--------------------------------------------------------------------------------

data PgColumn = PgColumn Symbol PgTypeSpec

infix 8 :&
infix 4 :*
infix 4 :@

-- | A type with the given list of constraints.
type ty :& constraints = 'PgConstrained ty constraints

-- | A column with with the given name and constrained type.
type name :* ts = 'PgColumn name ts

-- | A non-null column with the given name and type
type name :@ ty = name :* ty :& '[ 'PgNotNull ]

-- | An unconstrained (so nullable) column with the given name and type
type name :? ty = name :* 'PgSimple ty


-- | A table in the database
data PgTable = PgTable Symbol [PgColumn]

-- | A schema containing multiple tables
data PgSchema = PgSchema Symbol [PgTable]
