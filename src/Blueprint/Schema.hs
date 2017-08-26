{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE EmptyCase                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

module Blueprint.Schema where

import           Data.Singletons.Prelude
import           Data.Singletons.TH
import           Data.Singletons.TypeLits

--------------------------------------------------------------------------------
--  Postgres Types
--------------------------------------------------------------------------------

-- type Precision = Number
-- type Scale = Number

type Name = Symbol

$(singletons
 [d|

  type Precision' nat = nat
  type Scale' nat = nat

  -- | The kind of PostgreSQL data types. In practice use 'PgType'.
  data PgType' nat =
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
    | PgNumeric (Precision' nat) (Scale' nat)
      -- ^ Arbitrary-precision decimal number with given number of digits total
      -- ('prec') and number of digits after the decimal point ('scale').

    -- Sized vectors --

    | PgBit nat              -- ^ Fixed-length bit vector
    | PgBitVarying nat       -- ^ Variable-length bit vector
    | PgCharacter nat        -- ^ Fixed-length character vector
    | PgCharacterVarying nat -- ^ Variable-length character vector

    -- Binary blobs --

    | PgBytea -- ^ byte array
    | PgText  -- ^ text blob

    | PgJson     -- ^ textual JSON
    | PgJsonb    -- ^ Binary JSON
    | PgXml      -- ^ XML data
    | PgTsquery  -- ^ text search query
    | PgTsvector -- ^ text search document

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
    | PgTimeWithoutTimeZone (Maybe (Precision' nat))
      -- ^ time of day (no time zone)
    | PgTimeWithTimeZone (Maybe (Precision' nat))
      -- ^ time of day, including time zone
    | PgTimestampWithoutTimeZone (Maybe (Precision' nat))
      -- ^ date and time (no time zone)
    | PgTimestampWithTimeZone (Maybe (Precision' nat))
      -- ^ date and time, including time zone
    | PgInterval (Maybe (Precision' nat))
      -- ^ time span

    -- TODO: interval quantity units

    -- Misc --

    | PgPgLsn        -- ^ PostgreSQL Log Sequence Number
    | PgTxidSnapshot -- ^ user-level transaction ID snapshot
    | PgUuid         -- ^ Universal unique identifier

  --------------------------------------------------------------------------------
  --  Constraints
  --------------------------------------------------------------------------------

  data PgTableRef

  data PgConstraint =
      PgUnique
    | PgNotNull
    | PgPrimaryKey
    | PgReferences PgTableRef

  -- | The kind of PostgreSQL column type specifications. In practice use
  -- 'PgTypeSpec'.
  data PgTypeSpec' nat
    = PgSimple (PgType' nat)
    | PgConstrained (PgType' nat) [PgConstraint]

  --------------------------------------------------------------------------------
  --  Tables
  --------------------------------------------------------------------------------

  -- | The kind of PostgreSQL columns. In practice use 'PgColumn'.
  data PgColumn' nat name =
    PgColumn name (PgTypeSpec' nat)

  -- | The kind of PostgreSQL tables. In practice use 'PgTable'.
  data PgTable' nat name =
    PgTable name [PgColumn' nat name]

  -- | The kind of PostgreSQL schemas. In practice use 'PgSchema'.
  data PgSchema' nat name =
    PgSchema name [PgTable' nat name]

 |])

type Precision = Precision' Nat
type Scale = Scale' Nat

type PgType     = PgType' Nat
type PgTypeSpec = PgTypeSpec' Nat
type PgColumn   = PgColumn' Nat Name
type PgTable    = PgTable' Nat Name
type PgSchema   = PgSchema' Nat Name

type SPgType     = SPgType'
type SPgTypeSpec = SPgTypeSpec'
type SPgColumn   = SPgColumn'
type SPgTable    = SPgTable'
type SPgSchema   = SPgSchema'

$(singletonsOnly
 [d|

  infix 8 -&
  infix 4 -*
  infix 4 -@
  infix 4 -?

  -- | A type with the given list of constraints.
  (-&) :: PgType -> [PgConstraint] -> PgTypeSpec
  ty -& constraints = PgConstrained ty constraints

  -- | A column with with the given name and constrained type.
  (-*) :: Name -> PgTypeSpec -> PgColumn
  name -* ts = PgColumn name ts

  -- | A non-null column with the given name and type
  (-@) :: Name -> PgType -> PgColumn
  name -@ ty = name -* (ty -& [PgNotNull])

  -- | An unconstrained (so nullable) column with the given name and type
  (-?) :: Name -> PgType -> PgColumn
  name -? ty = name -* PgSimple ty


  --------------------------------------------------------------------------------
  --  Synonyms for common PostgreSQL types
  --------------------------------------------------------------------------------

  pgTimestamp :: PgType
  pgTimestamp = PgTimestampWithTimeZone Nothing

  pgTimestamp' :: PgType
  pgTimestamp' = PgTimestampWithoutTimeZone Nothing

 |])
