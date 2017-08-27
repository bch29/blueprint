{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE TypeFamilies          #-}

module Blueprint.Labels where

import           GHC.OverloadedLabels
import           GHC.TypeLits

data LabelProxy (n :: Symbol) = LabelProxy

instance (n ~ n') => IsLabel n (LabelProxy n') where
  fromLabel = LabelProxy
