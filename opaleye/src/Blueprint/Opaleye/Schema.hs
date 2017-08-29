module Blueprint.Opaleye.Schema
  (
    -- * SQL blueprints
    TableBlueprint
  , SchemaBlueprint(..)
    -- * Blueprint kinds
  , BpkTable(..)
  , T
    -- * Re-exports from "Blueprint"
  , Blueprint
  , (:@)
  , Mapping(..)
  , (:->)
  ) where

import           Typemap

import           Blueprint.Core

import           Blueprint.Opaleye.Schema.Core

