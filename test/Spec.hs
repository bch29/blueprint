{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main where

import WebPrelude

import Data.Singletons

import Blueprint.Schema
import Blueprint.Query


type Users = 'PgTable "users"
  '[ "id"           :-@ 'PgUuid
   , "name"         :-@ 'PgText
   , "age"          :-@ 'PgInteger
   , "phone_number" :-@ 'PgCharacterVarying 20
   ]

type Products = 'PgTable "products"
  '[ "id"    :-@ 'PgUuid
   , "name"  :-@ 'PgText
   , "price" :-@ 'PgReal
   ]

type Purchases = 'PgTable "purchases"
  '[ "user_id"    :-@ 'PgUuid
   , "product_id" :-@ 'PgUuid
   , "date"       :-@ PgTimestamp'
   ]


type PublicSchema = 'PgSchema "public"
  '[ Users
   , Products
   , Purchases
   ]


qUserPurchasedItems =
  (table @Users .*.
   (table @Products
    & renames @[ "id" :=> "products_product_id"
               , "name" :=> "product_name"
               ]
   ) .*. table @Purchases)
  & restrict (eqCols @"user_id" @"id")
  & restrict (eqCols @"product_id" @"products_product_id")
  & project @["id", "name", "age", "phone_number", "product_name"]


cUserPurchasedItems = sQueryColumns qUserPurchasedItems


schemaSing :: Sing PublicSchema
schemaSing = sing


main :: IO ()
main = putStrLn "Test suite not yet implemented"
