{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE EmptyCase                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Blueprint.Query where

-- import           WebPrelude

import           Data.Maybe (catMaybes)

import           Data.Singletons.Prelude
import           Data.Singletons.Prelude.List
import           Data.Singletons.Prelude.Maybe
import           Data.Singletons.TH
import           Data.Singletons.TypeLits

import           Blueprint.Schema

--------------------------------------------------------------------------------
--  Queries
--------------------------------------------------------------------------------

$(singletons
 [d|
  data Query' nat name =

      -- Primitives --

      QTable (PgTable' nat name)

      -- Operations --

    | QProject [name] (Query' nat name)
    | QRename  [(name, name)] (Query' nat name)
    | QProduct (Query' nat name) (Query' nat name)
    | QRestrict (Restriction' name) (Query' nat name)

  data Restriction' name =
    RColsEqual name name

 |])

type Query = Query' Nat Symbol

--------------------------------------------------------------------------------
--  Utility
--------------------------------------------------------------------------------

infixr 4 .:

(.:) :: Sing x -> Sing xs -> Sing (x : xs)
(.:) = SCons

nil :: Sing '[]
nil = SNil

--------------------------------------------------------------------------------
--  Query builders
--------------------------------------------------------------------------------

infixr 4 .*.

table :: SingI table => Sing (QTable table)
table = SQTable sing

project :: SingI names => Sing query -> Sing (QProject names query)
project = SQProject sing

project1 :: SingI name => Sing query -> Sing (QProject '[name] query)
project1 = project

renames :: SingI renames => Sing query -> Sing (QRename renames query)
renames = SQRename sing

rename
  :: (SingI original, SingI new)
  => Sing query
  -> Sing (QRename '[ '(original, new)] query)
rename = renames

restrict :: Sing restriction -> Sing query -> Sing (QRestrict restriction query)
restrict = SQRestrict

(.*.) :: Sing q1 -> Sing q2 -> Sing (q1 `QProduct` q2)
(.*.) = SQProduct

(.==) :: Sing col1 -> Sing col2 -> Sing (RColsEqual col1 col2)
(.==) = SRColsEqual

eqCols :: (SingI col1, SingI col2) => Sing (RColsEqual col1 col2)
eqCols = sing .== sing

symb :: forall (s :: Symbol). SingI s => Sing s
symb = sing

type (:=>) a b = '(a, b)

--------------------------------------------------------------------------------
--  Running Queries
--------------------------------------------------------------------------------

$(singletonsOnly
 [d|

  projectColumns :: [Symbol] -> [PgColumn] -> [PgColumn]
  projectColumns names columns = catMaybes projectedMaybes
    -- It would be easier to implement this as a filter, but that wouldn't put
    -- the new columns in the order of the requested projections.
    where
      colPairs = map (\c@(PgColumn n _) -> (n, c)) columns
      projectedMaybes = map (\n -> lookup n colPairs) names

  renameColumns :: [(Symbol, Symbol)] -> [PgColumn] -> [PgColumn]
  renameColumns rs =
    map (\c@(PgColumn n t) -> case lookup n rs of
            Just n' -> PgColumn n' t
            Nothing -> c)


  queryColumns :: Query -> [PgColumn]
  queryColumns = \case
    QTable (PgTable _ cols) -> cols
    QProject columnNames query ->
      projectColumns columnNames (queryColumns query)
    QRename rs query ->
      renameColumns rs (queryColumns query)
    QProduct query1 query2 -> queryColumns query1 ++ queryColumns query2

    -- A restriction keeps the set of columns in the output the same, it just
    -- reduces the number of rows.
    QRestrict _ query -> queryColumns query
 |]
 )
