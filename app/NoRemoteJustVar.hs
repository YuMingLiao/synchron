{-# Language OverloadedStrings #-}
import Prelude hiding (div)
import Syn
import Data.Text as T
import Var
import Replica.DOM hiding (var)
import           Data.Semigroup (Last (..))
import           Replica.Events
import           Replica.Props hiding     (async, loop)

inputOnEnter p v = do
  e <- input [ autofocus True, placeholder p, value v, Left <$> onInput, Right <$> onKeyDown ]
  case e of
    Left e  -> inputOnEnter p (targetValue $ target e)
    Right e -> if kbdKey e == "Enter"
      then pure v
      else inputOnEnter p v

counter x = do
  div [ onClick ] [ text (T.pack $ show x) ]
  counter (x + 1)



reactText v = loop v $ stream $ \s -> do
  div [ onClick ] [ text "CLICK ME!!!" ]
  counter 0
  text (T.pack $ show s)


testNoRemote = do
  runReplica $ pool $ \p -> var (Last "") $ \v -> do
    spawn p (go v)
    spawn p (reactText v)
    spawn p (reactText v)
    spawn p (reactText v)
    spawn p (reactText v)
    spawn p (reactText v)
    Syn.forever
  where
    go v = do
      s <- div [] [ inputOnEnter "" "" ]
      putVar v (Last s)
      go v

main = testNoRemote
