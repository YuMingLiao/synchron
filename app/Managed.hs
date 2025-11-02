module Managed where
import Syn hiding (await, emit)
import qualified Syn (await, emit)
import Var hiding (var)
import qualified Var (var)
import Control.Monad.IO.Class 
import Control.Applicative (liftA2)
import Replica.DOM hiding (var)
import Control.Monad

newtype Managed v a = Managed { (>>-) :: forall r. (a -> Syn v r) -> Syn v r }

-- the original a in Managed, a.k.a x
-- first, f it.
-- then, return_ it.
-- use lambda to get x from mx
-- then use lambda to leave room for (a -> Syn v r)
-- You can still have a ConT style f while also get "functored"
--
instance Functor (Managed v) where
    fmap f mx = Managed (\return_ ->
        mx >>- \x ->
        return_ (f x) )

instance Applicative (Managed v) where
    pure r    = Managed (\return_ ->
        return_ r )

    mf <*> mx = Managed (\return_ ->
        mf >>- \f ->
        mx >>- \x ->
        return_ (f x) )

instance Monad (Managed v) where
    ma >>= f = Managed (\return_ ->
        ma  >>- \a ->
        f a >>- \b ->
        return_ b )

instance MonadIO (Managed v) where
    liftIO m = Managed (\return_ -> do
        a <- effect $ m
        return_ a )

class MonadSyn m where
  liftSyn :: Syn v a -> m v a

-- seems lift Syn into Managed couldn't make it sequencial exetuable. 

instance MonadSyn Managed where
    liftSyn m = Managed (\return_ -> do
        a <- m
        return_ a )

instance Semigroup a => Semigroup (Managed v a) where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (Managed v a) where
    mempty = pure mempty

class MonadSyn m => MonadManaged m where
    using :: Managed v a -> m v a

instance MonadManaged Managed where
    using = id 

-- | Build a `Managed` value
managed :: (forall v r. (a -> Syn v r) -> Syn v r) -> Managed v a
managed f = using (Managed f)

runManaged :: Monoid v => Managed v a -> Syn v a
runManaged m = m >>- return


runManagedHTML :: Managed HTML a -> Syn HTML a
runManagedHTML = runManaged

emit :: Event Internal a -> a -> Syn HTML ()
emit = Syn.emit

await :: Event t a -> Syn HTML a
await = Syn.await


var :: (Semigroup a, Monoid v) => a -> (Var a -> Syn v b) -> Syn v b 
var = Var.var
-- runReplica foo
foo :: Syn HTML ()
foo = do
  runManaged $ do
    e <- managed local
    r <- liftSyn $ orr [await e, emit e ()]
    liftSyn . io $ print r
    pure r

-- runReplica (bar >> pure ())
bar :: Syn HTML ((),())
bar = do
  runManagedHTML $ do
    e1 <- managed local
    e2 <- managed local
    -- var can't offer a v with Monoid v. And offer HTML is not enough.
    -- v <- managed (var "a")
    r <- liftSyn $ andd (emit e1 (), emit e2 ())
    liftSyn . io $ print r
    pure r

-- since local $ \e -> can be aligned, so I don't see the usefulness of runManaged.
