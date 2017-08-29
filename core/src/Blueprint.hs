module Blueprint
  (
    -- * Blueprints
    Blueprint
  , SBlueprint
  , (:@)
  , SimpleBP
  , (:->)
    -- * Blueprint Kinds
  , BpkSimple(..)
  , S
    -- * Records
  , module R
    -- * Lenses into records
  , module L
  ) where

import           Blueprint.Lens   as L
import           Blueprint.Record as R
import           Blueprint.Core
import           Typemap
