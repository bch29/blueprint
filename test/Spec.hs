{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main where

import WebPrelude

import Opaleye

import Blueprint.Internal.Schema
import Blueprint.Internal.Records


type Users = 'SchemaTable "users"
  '[ "id"           :@ UUID
   , "name"         :@ Text
   , "age"          :@ Int
   , "phone_number" :@ Maybe Text
   ]

type Products = 'SchemaTable "products"
  '[ "id"    :@ UUID
   , "name"  :@ Text
   , "price" :@ Double
   ]

type Purchases = 'SchemaTable "purchases"
  '[ "user_id"    :@ UUID
   , "product_id" :@ UUID
   , "date"       :@ UTCTime
   ]


type PublicSchema = 'Schema "public"
  '[ Users
   , Products
   , Purchases
   ]


usersTable :: Table (ColumnsOf Users) (ColumnsOf Users)
usersTable = table'

productsTable :: Table (ColumnsOf Products) (ColumnsOf Products)
productsTable = table'

purchasesTable :: Table (ColumnsOf Purchases) (ColumnsOf Purchases)
purchasesTable = table'


-- usersQuery = queryTable usersTable


main :: IO ()
main = putStrLn "Test suite not yet implemented"
