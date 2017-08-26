{-# LANGUAGE PatternSynonyms #-}

module WebPrelude
  (
    -- * Basic Types and Classes
    Typeable
  , Data
  , Generic
  , Proxy(..)
  , MonadIO(..)
    -- * Basic Combinators
  , (<>)
  , void
  , forM_
  , traverse_
    -- * Data Types
  , Text.Text
  , BS.ByteString
  , Map.Map
  , UUID
  , Scientific

    -- * Time
  , UTCTime(..)
  , Day(..)
  , NominalDiffTime

    -- * Lens
    -- ** Combinators
  , re
  , from
  , to

  , view
  , review

  , use

  , traversed
  , each
  , folded
  , mapped

  , itraversed

  , at
  , ix

  , _Just
  , _Wrapped
    -- ** Operators
  , (&)
  , (#)

  , (^.)
  , (^?)
  , (^..)

  , (.~)
  , (%~)
  , (%%~)

  , (.=)
  , (%=)
    -- ** Types
  , Lens
  , Lens'
  , Traversal
  , Traversal'
  , Prism
  , Prism'

  , IndexedLens
  , IndexedLens'
  , IndexedTraversal
  , IndexedTraversal'
    -- ** Tuples
  , module LensTuple
    -- ** Text
  , IsText(..)
  , unpacked
  , _Text
  , pattern Text
    -- ** Template Haskell
  , module LensTH
  ) where

import           Control.Monad
import           Data.Foldable
import           Control.Monad.IO.Class
import           Data.Data              (Data, Proxy (..), Typeable)
import           Data.Monoid
import           GHC.Generics           (Generic)

import qualified Data.ByteString        as BS
import qualified Data.Map.Strict        as Map
import qualified Data.Text              as Text

import           Control.Lens
import           Control.Lens.TH        as LensTH
import           Control.Lens.Tuple     as LensTuple
import           Data.Text.Lens

import           Data.Scientific        (Scientific)
import           Data.Time
import           Data.UUID.Types        (UUID)
