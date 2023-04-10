{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveAnyClass #-}

module RecordVar where

import Data.Either (lefts, rights)
import Data.List.NonEmpty
import Data.Semigroup

import Syn
import Var
import Data.Generic.HKD
import GHC.Generics
import Data.Typeable
import Control.Lens ((.~), (^.), (&), Const (..), Identity, anyOf)
import Generics.SOP

data A = A { a :: Int, b :: Char} | B deriving (GHC.Generics.Generic, Generics.SOP.Generic, Show)
type RecordVar a = HKD a Var 
type Labels a = HKD a (Const String)
instance Typeable a => Show (Var a) where
  show _ = "Var " ++ show (typeOf (undefined ::a))

-- I intend to use a record initial value to create a record full of vars. But I don't know how to chain local' and get eventName. Should I put them into the same pool? According to syntax, I don't have to. But how could I get a Rank2-like argument into a Var e and assign it to a field?
-- ideas: Locals, then map record, then sequence in recordVar
collectEs :: Semigroup a => Syn () [Event Internal a]
collectEs = local' (<>) $ \e1 -> local' (<>) $ \e2 -> pure [e1,e2] 
{-
recordVar :: All2 Semigroup (Code a) => Monoid v => a -> (RecordVar a -> Syn v b) -> Syn v b
recordVar a@{..} = local' (<>) $ \e1 -> local' (<>) $ \e2 -> pool $ \p -> do
  spawn p (trail (field1 a) e1)  
  spawn p (trail (field2 a) e2)  
  f (Record { field1 = Var e1, field2 = Var e2 })
  where
    trail a e = undefined {- the same -}
    -}
var :: Semigroup a => Monoid v => a -> (Var a -> Syn v b) -> Syn v b
var a f = local' (<>) $ \e -> pool $ \p -> do
  spawn p (trail a e)
  f (Var e)
  where
    trail a e = do
      r <- await e

      let a' = case sconcat <$> nonEmpty (lefts r) of
                 Just a' -> a'
                 Nothing -> a
            
          es = case rights r of
                 [] -> [pure ()]
                 es -> fmap (flip emit a') es

      snd <$> andd (andd' es, trail a' e)

