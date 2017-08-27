{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Blueprint.Records.Opaleye where

import           Data.Profunctor

import           Opaleye.Column
import           Opaleye.Table

import           Data.Singletons

import           Blueprint.Records

--------------------------------------------------------------------------------
--  Constructing Opaleye Tables
--------------------------------------------------------------------------------

type TableOf table = Table (ColSqlOf Column table) (ColSqlOf Column table)

table' :: SingI table => TableOf table
table' = makeTable (dimap getColSql ColSql . required) Table

tableFor
  :: SingI table
  => proxy table -> TableOf table
tableFor _ = table'

