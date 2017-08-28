{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}

module Blueprint.Labels where

import           GHC.OverloadedLabels

data ColKey (key :: k) = ColKey

instance (k ~ k') => IsLabel k (ColKey k') where
  fromLabel = ColKey
