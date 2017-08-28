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

import           Data.Aeson
import           Opaleye

import           Blueprint


type Users = 'SchemaTable "users"
  '[ "id"           :-> Int
   , "age"          :-> Int
   , "phone_number" :-> Int
   ]

type Products = 'SchemaTable "products"
  '[ "id"    :-> Int
   , "name"  :-> Text
   , "price" :-> Double
   ]

type Purchases = 'SchemaTable "purchases"
  '[ "user_id"    :-> Int
   , "product_id" :-> Int
   , "date"       :-> Int
   ]


type PublicSchema = 'Schema "public"
  '[ Users
   , Products
   , Purchases
   ]


testProduct :: RecordOf Products
testProduct = TRec $
  record
  & #id    =: (0 :: Int)
  & #name  =: "test_product"
  & #price =: (0.0 :: Double)


usersTable :: TableOf Users
usersTable = table' @Users

productsTable :: TableOf Products
productsTable = table' @Products

purchasesTable :: TableOf Purchases
purchasesTable = table' @Purchases


usersQuery :: Query (ColumnsOf Users)
usersQuery = queryTable usersTable


main :: IO ()
main = putStrLn "Test suite not yet implemented"
