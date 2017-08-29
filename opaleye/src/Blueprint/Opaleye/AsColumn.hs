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

module Blueprint.Opaleye.AsColumn where

import           Data.Int             (Int16, Int32, Int64)

import           Data.CaseInsensitive (CI)
import           Data.Scientific      (Scientific)
import           Data.Text            (Text)
import           Data.Time            (Day, LocalTime, TimeOfDay, UTCTime)
import           Data.UUID.Types      (UUID)

import           Opaleye.Column       (Nullable)
import           Opaleye.PGTypes

--------------------------------------------------------------------------------
--  Class
--------------------------------------------------------------------------------

class (IsSqlType (SqlType a)) => AsColumn a where
  type SqlType a

--------------------------------------------------------------------------------
--  Combinatory Instances
--------------------------------------------------------------------------------

instance AsColumn a => AsColumn (Maybe a) where
  type SqlType (Maybe a) = Nullable (SqlType a)

--------------------------------------------------------------------------------
--  Concrete Instances
--------------------------------------------------------------------------------

instance AsColumn Bool where type SqlType Bool = PGBool

instance AsColumn Int16 where type SqlType Int16 = PGInt2
instance AsColumn Int32 where type SqlType Int32 = PGInt4
instance AsColumn Int64 where type SqlType Int64 = PGInt8
instance AsColumn Int where type SqlType Int = PGInt4

instance AsColumn Float where type SqlType Float = PGFloat4
instance AsColumn Double where type SqlType Double = PGFloat8

instance AsColumn Scientific where type SqlType Scientific = PGNumeric

instance AsColumn Day where type SqlType Day = PGDate
instance AsColumn TimeOfDay where type SqlType TimeOfDay = PGTime
instance AsColumn LocalTime where type SqlType LocalTime = PGTimestamp
instance AsColumn UTCTime where type SqlType UTCTime = PGTimestamptz

instance AsColumn Text where type SqlType Text = PGText
instance AsColumn (CI Text) where type SqlType (CI Text) = PGCitext

instance AsColumn UUID where type SqlType UUID = PGUuid
