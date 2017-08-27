{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}

module Blueprint.Records.Bookkeeper
  ( fromBook
  , ColsToMapping
  ) where

import           Data.Vinyl

import           Data.Singletons
import           Data.Singletons.Prelude.List (Sing (..))

import           Blueprint.Internal.Schema
import           Blueprint.Internal.Vinyl
import           Blueprint.Records

import           Bookkeeper.Internal

import           Data.Type.Map                (Map (..))
import qualified Data.Type.Map as Map
import qualified Data.Type.Set as Set


fromBook
  :: ( ColsToMapping cols ~ mapping
     , Map.Submap mapping mapping'
     , SingI cols
     )
  => Book' mapping' -> RecordOf ('SchemaTable tname cols)
fromBook (Book x) = fromTypeMap x


fromTypeMap
  :: ( ColsToMapping cols ~ mapping
     , Map.Submap mapping mapping'
     , SingI cols
     )
  => Map mapping' -> RecordOf ('SchemaTable tname cols)
fromTypeMap = TRec . recFromTypeMap'


recFromTypeMap'
  :: ( ColsToMapping cols ~ mapping
     , Map.Submap mapping mapping'
     , SingI cols
     )
  => Map mapping' -> Rec (OverCol Identity) cols
recFromTypeMap' xs = recFromTypeMap sing (Map.submap xs)


recFromTypeMap
    :: (ColsToMapping cols ~ mapping)
    => Sing cols
    -> Map mapping -> Rec (OverCol Identity) cols
recFromTypeMap = \case
  SNil -> \case
    Empty -> RNil
  SCons (SSchemaColumn _ _) sCols -> \case
    Ext _ x xs -> OverCol (Identity x) :& recFromTypeMap sCols xs

type family ColsToMapping cols = mapping | mapping -> cols where
  ColsToMapping '[] = '[]
  ColsToMapping ((name :@ ty) ': cols) = (name :=> ty) ': ColsToMapping cols
