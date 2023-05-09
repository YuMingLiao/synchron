{-# Language DataKinds #-}
{-# Language TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
import Data.Tuple.Curry
import qualified Data.Function.Poly as FP
import Data.Tuple.HList

import Data.Tuple.Solo
import GHC.TypeLits

type family Tuple (xs :: [*]) :: * where
  Tuple '[] = TypeError (Text "impossible HListToTuple")
  Tuple (a1 ': '[]) = Solo a1
  Tuple (a1 ': a2 ': '[]) = (a1, a2)
  Tuple (a1 ': a2 ': a3 ': '[]) = (a1, a2, a3)
  Tuple (a1 ': a2 ': a3 ': a4 ': '[]) = (a1, a2, a3, a4)
  Tuple (a1 ': a2 ': a3 ': a4 ': a5 ': '[]) = (a1, a2, a3, a4, a5)

-- so uncurryN can make f with any arguments into a -> b  wher a is (a1, a2, ...)
-- but pattern match on n-ary tuple or n-args func is still unavailable.
--
