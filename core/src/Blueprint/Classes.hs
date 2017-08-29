{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Blueprint.Classes where

import           Data.Singletons

import           Blueprint.Core
import           Blueprint.Internal.Map

--------------------------------------------------------------------------------
--  Classes
--------------------------------------------------------------------------------

-- | Describes how to show records made from blueprints with extra data of a
-- particular kind.
class FormatShowBlueprint f t where
  -- | Given a list of field names and the values in those fields, format the
  -- fields to show the record.
  formatShowRecord :: (SingI (d :: t)) => proxy d -> [(String, Some Show f)] -> String


--------------------------------------------------------------------------------
--  Instances
--------------------------------------------------------------------------------

-- | E.g.
--
-- @
-- record
-- & #name =: "Brad"
-- & #age =: 21
-- @
--
instance FormatShowBlueprint Identity BpkSimple where
  formatShowRecord _ =
    ("record " ++ ) .
    concatMap (\(k, Some (Identity v)) -> "\n& #" ++ k ++ " =: " ++ show v)

--------------------------------------------------------------------------------
--  Other
--------------------------------------------------------------------------------

data Some c f where
  Some :: c a => f a -> Some c f
