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

p1 = local $ \e -> do
  a <- orr [ Left <$> await e, Right <$> await e]
  pure a

test :: (Show a, Eq a) => Syn () a -> a -> Assertion
test f a = ((fromJust . fst) <$> exhaust localNid f) >>= (@?= a)

main :: IO ()
main = defaultMain $ testGroup "Example tests"
  [ testCase "p1" $ test p1 (Left "B")
  ]
