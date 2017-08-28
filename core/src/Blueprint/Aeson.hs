{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Blueprint.Aeson where

import           Data.Kind                    (Type)

import           Control.Lens.At
import           Control.Lens.Getter
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Text                    as Text

import           Typemap
import qualified Typemap.Combinators          as Map
import           Typemap.Mapping
import           Typemap.TypeLevel

import           Data.Singletons
import           Data.Singletons.Prelude.List (Sing (..))
import           Data.Singletons.TypeLits

import           Blueprint.Internal.Map

--------------------------------------------------------------------------------
--  From JSON
--------------------------------------------------------------------------------

mapFromJSON
  :: (ValsSatisfy FromJSON m)
  => Object -> Sing (m :: [Mapping Symbol Type])
  -> Parser (Map Identity m)
mapFromJSON _ SNil = return Empty
mapFromJSON o (SCons (SMapping k (_ :: Proxy a)) xs) = do
  let k' = fromSing k
  x :: a <- case o ^. at k' of
    Just x -> parseJSON x
    Nothing -> fail $ "key '" ++ Text.unpack k' ++ "' not present in JSON"

  s <- mapFromJSON o xs

  return (Ext k (Identity x) s)

--------------------------------------------------------------------------------
--  To JSON
--------------------------------------------------------------------------------

mapToJSON
  :: (ValsSatisfy ToJSON m)
  => Map Identity (m :: [Mapping Symbol Type])
  -> Value
mapToJSON =
  object .
  Map.toList .
  Map.mapWithKey (\k (Map.Constrained v) -> Const (fieldToJSON k v)) .
  (Map.constrained @ToJSON)

fieldToJSON :: (ToJSON v) => Sing (k :: Symbol) -> Identity v -> Pair
fieldToJSON k (Identity x) = fromSing k .= x
