import Data.Maybe (fromJust)
import Data.IORef
import Control.Concurrent
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

-- Well, orr io is meant for replica. does exhaust needs stepAll all ios before blocked?
p1 = local $ \e -> do
  a <- orr [ effect (do threadDelay 2000000; pure 1;) , effect (do threadDelay 1000000; pure 2;)]
  pure a

test :: (Show a, Eq a) => Syn () a -> a -> Assertion
test f a = ((fromJust . fst) <$> exhaust localNid f) >>= (@?= a)

main :: IO ()
main = defaultMain $ testGroup "Example tests"
  [ testCase "p1" $ test p1 2
  ]
