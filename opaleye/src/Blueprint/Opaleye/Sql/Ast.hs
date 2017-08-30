{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Blueprint.Opaleye.Sql.Ast where

import Data.Text (Text)

import Opaleye.Internal.HaskellDB.Sql

import Blueprint


type SqlCreateTable = SimpleBP
  '[ "name" :-> SqlTable
   , "useIfNotExists" :-> Bool
   , "columns" :-> [Record SqlColumnSpec]
   ]

type SqlColumnSpec = SimpleBP
  '[ "name" :-> Text
   , "type" :-> Text
   , "constraints" :-> [SqlColumnConstraint]
   ]

data SqlColumnConstraint
  = SccUnique
  | SccNotNull


-- columnSQL :: AsColumn a => Sing (n :-> a)
