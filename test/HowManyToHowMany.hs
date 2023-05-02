import Data.Maybe (fromJust)
import Data.IORef

import Test.Tasty
import Test.Tasty.HUnit

import Syn hiding (emit, await)
import qualified Syn

localNid :: NodeId
localNid = NodeId 0

emit :: Event Internal a -> a -> Syn () ()
emit = Syn.emit

await :: Event t a -> Syn () a
await = Syn.await

-- because Event const, two es become one e "2". So at logical time, one emit e, many await e.
-- At a logical time, event value is decided by conc in Event conc. const: the leftest. max, min, .... 
-- So how returnable Async inform stepAll to unblock? 
p1 = local $ \e -> do
  a <- andd (await e, emit e "1", emit e "2", await e)
  pure a

-- no await for this e
p2 = local $ \e -> local $ \f -> do
  a <- andd (emit e "e", emit f "f", await f)
  pure a

-- spawn await then emit
-- await in spawn needs event to return value.
--
p3 = pool $ \p -> local $ \i -> local $ \o -> do
  spawn p (go i o)
  emit i "i"
  await o -- you don't need andd or orr to return value.
  where
    go i o = do
      a <- await i
      emit o a

test :: (Show a, Eq a) => Syn () a -> a -> Assertion
test f a = ((fromJust . fst) <$> exhaust localNid f) >>= (@?= a)

main :: IO ()
main = defaultMain $ testGroup "Example tests"
  [ testCase "p1" $ test p1 ("2", (), (), "2")
  , testCase "p2" $ test p2 ((), (), "f")
  , testCase "p3" $ test p3 "i"
  ]
