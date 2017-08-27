{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE TypeFamilies          #-}

module Blueprint.Labels where

import           GHC.OverloadedLabels
import           GHC.TypeLits

data ColumnAccessor (n :: Symbol) = ColumnAccessor

instance (n ~ n') => IsLabel n (ColumnAccessor n') where
  fromLabel = ColumnAccessor
