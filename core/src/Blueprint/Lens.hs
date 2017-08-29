{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Blueprint.Lens
  (
    -- * Lenses for records
    flens
  , vlens
  , _Rec
  ) where

import           Control.Lens.Lens

import           Blueprint.Core
import           Blueprint.Internal.Map
import           Blueprint.Labels
import           Blueprint.Record

import           Typemap
import           Typemap.Lens


--------------------------------------------------------------------------------
--  Lenses for tables
--------------------------------------------------------------------------------

{-|

Lens into a functorial member of a general blueprinted record.

> flens :: (HasKeyAt k (MappingOf b) r a) => BlueKey k -> Lens' (Rec f b) (f a)

-}
flens
  :: (b ~ (d :@ m), HasKeyAt k m r a)
  => BlueKey k
  -> Lens' (Rec' f b) (f a)
flens p = _Rec . klens p

{-|

Lens into a value member of a 'Record'.

> vlens :: (HasKeyAt k (MappingOf b) r a) => BlueKey k -> Lens' (Record b) a

-}
vlens
  :: ( b ~ (d :@ m)
     , HasKeyAt k m r a)
  => BlueKey k -> Lens' (Record' b) a
vlens p = flens p . _Identity

--------------------------------------------------------------------------------
--  Lenses for newtypes
--------------------------------------------------------------------------------

-- | Lens into a record's underlying 'Map'.
_Rec :: (b ~ (d :@ m)) => Lens (Rec' f b) (Rec' g b) (Map f m) (Map g m)
_Rec f (Rec s) = fmap Rec (f s)

_Identity :: Lens (Identity a) (Identity b) a b
_Identity f (Identity x) = fmap Identity (f x)
