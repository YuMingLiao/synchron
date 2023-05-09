module Managed where
import Syn
import Control.Monad.IO.Class 
import Control.Applicative (liftA2)
import Replica.DOM
import Control.Monad

newtype Managed a = Managed { (>>-) :: forall v r. (a -> Syn v r) -> Syn v r }

-- the original a in Managed, a.k.a x
-- first, f it.
-- then, return_ it.
-- use lambda to get x from mx
-- then use lambda to leave room for (a -> Syn v r)
-- You can still have a ConT style f while also get "functored"
--
instance Functor Managed where
    fmap f mx = Managed (\return_ ->
        mx >>- \x ->
        return_ (f x) )

instance Applicative Managed where
    pure r    = Managed (\return_ ->
        return_ r )

    mf <*> mx = Managed (\return_ ->
        mf >>- \f ->
        mx >>- \x ->
        return_ (f x) )

instance Monad Managed where
    ma >>= f = Managed (\return_ ->
        ma  >>- \a ->
        f a >>- \b ->
        return_ b )

instance MonadIO Managed where
    liftIO m = Managed (\return_ -> do
        a <- effect $ m
        return_ a )

class MonadSyn m where
  liftSyn :: Syn v a -> m a

{- turn a Syn to a Managed. What about v? use exhaust or run? 
instance MonadSyn Managed where
    liftSyn m = Managed (\return_ -> do
        a <- m
        return_ a )
-}
instance Semigroup a => Semigroup (Managed a) where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (Managed a) where
    mempty = pure mempty

class MonadIO m => MonadManaged m where
    using :: Managed a -> m a

instance MonadManaged Managed where
    using = id

-- | Build a `Managed` value
managed :: (forall v r. (a -> Syn v r) -> Syn v r) -> Managed a
managed f = using (Managed f)

runManaged :: Monoid v => Managed a -> Syn v a
runManaged m = m >>- return

foo :: Syn HTML (Syn HTML a)
foo = do
  runManaged $ do
    as <- replicateM 2 $ managed local
    pure (orr (map await as))

bar :: Syn HTML (Event Internal a)
bar = do
  e1 <- local $ return
  pure e1
