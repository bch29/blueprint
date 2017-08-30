{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-|

Generating SQL to create schemas.

-}
module Blueprint.Opaleye.Sql.Print where

-- import qualified Data.Text                         as Text

-- import           Control.Lens                      hiding (Identity)

-- import           Data.Profunctor                   (Profunctor (..))
-- import           Data.Profunctor.Product.Default

-- import           Data.Singletons

-- import           Opaleye.Column
-- import           Opaleye.Table

-- import           Typemap
-- import           Typemap.Lens                      (HasKeyAt (..))

-- import           Blueprint.Core
-- import           Blueprint.Labels
-- import           Blueprint.Lens
-- import           Blueprint.Record
-- import           Blueprint.Record.Profunctor

-- import           Blueprint.Opaleye.AsColumn
-- import           Blueprint.Opaleye.Schema.Core


-- newtype S


-- columnSQL :: AsColumn a => Sing (n :-> a)
