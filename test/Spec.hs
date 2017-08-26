{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Types


type Users = 'PgTable "users"
  '[ "id"           :@ 'PgUuid
   , "name"         :@ 'PgText
   , "age"          :@ 'PgInteger
   , "phone_number" :@ 'PgCharacterVarying 20
   ]

type Products = 'PgTable "products"
  '[ "id"    :@ 'PgUuid
   , "name"  :@ 'PgText
   , "price" :@ 'PgReal
   ]

type Purchases = 'PgTable "purchases"
  '[ "user_id"    :@ 'PgUuid
   , "product_id" :@ 'PgUuid
   , "date"       :@ PgTimestamp'
   ]


type PublicSchema = 'PgSchema "public"
  '[ Users
   , Products
   , Purchases
   ]


main :: IO ()
main = putStrLn "Test suite not yet implemented"
