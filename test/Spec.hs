{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE OverloadedLabels    #-}

{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main where

import           Opaleye
import           Bookkeeper

import           Blueprint


type Users = 'SchemaTable "users"
  '[ "id"           :@ Int
   , "age"          :@ Int
   , "phone_number" :@ Int
   ]

type Products = 'SchemaTable "products"
  '[ "price" :@ Double
   , "id"    :@ Int
   ]

type Purchases = 'SchemaTable "purchases"
  '[ "user_id"    :@ Int
   , "product_id" :@ Int
   , "date"       :@ Int
   ]


type PublicSchema = 'Schema "public"
  '[ Users
   , Products
   , Purchases
   ]


testProduct :: RecordOf Products
testProduct = fromBook $
  emptyBook
  & #price =: (0.0 :: Double)
  & #id    =: (0 :: Int)


usersTable :: Table (ColumnsOf Users) (ColumnsOf Users)
usersTable = table'

productsTable :: Table (ColumnsOf Products) (ColumnsOf Products)
productsTable = table'

purchasesTable :: Table (ColumnsOf Purchases) (ColumnsOf Purchases)
purchasesTable = table'


usersQuery = queryTable usersTable


main :: IO ()
main = putStrLn "Test suite not yet implemented"
