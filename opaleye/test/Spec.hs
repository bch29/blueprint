{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main where

import           Data.Text    (Text)

import           Control.Lens

import           Opaleye

import           Blueprint
import           Blueprint.Opaleye


type Users = T "users" :@
  '[ "id"           :-> Int
   , "age"          :-> Int
   , "phone_number" :-> Int
   ]

type Products = T "products" :@
  '[ "id"    :-> Int
   , "price" :-> Double
   , "name"  :-> Text
   ]

type Purchases = T "purchases" :@
  '[ "user_id"    :-> Int
   , "product_id" :-> Int
   , "date"       :-> Int
   ]


type PublicSchema = 'SchemaBP "public"
  '[ Users
   , Products
   , Purchases
   ]


testProduct :: Record Products
testProduct =
  record
  & #name  =: "test_product"
  & #id    =: (0 :: Int)
  & #price =: (0.0 :: Double)


usersTable :: TableOf Users
usersTable = blueprintTable @Users

productsTable :: TableOf Products
productsTable = blueprintTable @Products

purchasesTable :: TableOf Purchases
purchasesTable = blueprintTable @Purchases


usersQuery :: Query (ColumnsOf Users)
usersQuery = queryTable usersTable


main :: IO ()
main = putStrLn "Test suite not yet implemented"
