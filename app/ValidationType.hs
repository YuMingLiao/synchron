-- try eg5 in Higgledy as an easy solution for validation and error messages.
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

import Control.Applicative (Alternative (empty))
import Control.Lens ((.~), (^.), (&), Const (..), Identity, anyOf)
import Data.Generic.HKD
import Data.Maybe (isJust, isNothing)
import Data.Monoid (Last (..))
import GHC.Generics (Generic)
import Named ((:!), (!))

-- An example of a record (with named fields):
data User
  = User
      { name      :: String
      , age       :: Int
      , likesDogs :: Bool
      }
  deriving (Generic, Show)

type Partial a = HKD a  Last          -- Fields may be missing.
type ErrMsgs b a = HKD a (Either b)

user :: User
user = User "Tom" 26 True

eg4 :: ErrMsgs [String] User
eg4 = deconstruct @(Either [String]) user

eg5 :: Either [String] User
eg5 = construct eg4
--If any has Left, then leftmost Left. So it works for Maybe's Nothing.

