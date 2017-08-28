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

type ColumnsOf table = OverSqlOf Column table
type ColumnsOf' table = OverSqlOf' Column table

type TableOf' table = Table (ColumnsOf' table) (ColumnsOf' table)
type TableOf table = Table (ColumnsOf table) (ColumnsOf table)

table' :: forall table. SingI table => TableOf table
table' = tableForProxy (Proxy :: Proxy table)

tableForProxy
  :: SingI table
  => proxy table -> TableOf table
tableForProxy p =
  makeTable (singByProxy p) (dimap getOverSql OverSql . required) Table

