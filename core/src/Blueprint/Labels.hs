{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}

module Blueprint.Labels where

import           GHC.OverloadedLabels

data BlueKey (key :: k) = BlueKey

instance (k ~ k') => IsLabel k (BlueKey k') where
  fromLabel = BlueKey
