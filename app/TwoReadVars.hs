{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}


import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Monad (void)

import Control.Monad (forever)

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.Map as M
import qualified Data.Text as T
import           Data.Semigroup (Last (..))

import qualified Connector.WebSocket as WS

import qualified Connector.Log as Log
import qualified Connector.HTTP as HTTP

import           Replica.VDOM             (Attr(AText, ABool, AEvent, AMap), HTML, DOMEvent, VDOM(VNode, VText), defaultIndex, fireEvent)
import           Replica.VDOM.Types       (DOMEvent(DOMEvent), EventOptions(..))
import           Replica.DOM hiding       (var)
import           Replica.SVG
import           Replica.SVG.Props hiding (r)
import           Replica.Props hiding     (async, loop)
import           Replica.Events
import           Syn
import           Syn.SVG.Replica

import           Var

import Network.HTTP.Types.Status
import Network.WebSockets.Connection
import Network.Wai

import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.Replica as Replica

import Prelude hiding (div, forever, span)

import Debug.Trace
import System.IO.Unsafe
import Concur (runConcur, withPool, step)
import Control.Monad.IO.Class
import GHC.Generics
import Data.Generic.HKD
import Data.Text (Text)
import Control.Lens ((.~), (^.), (&), Const (..), Identity, anyOf)


main = runReplica twoReadVars

twoReadVars = var "a" $ \a -> var "b" $ \b -> loop b $ stream $ \b' -> do
 a <- readVar a 
 b <- readVar b
 async $ print a
 async $ print b
 button [] []
 pure $ Left ()

-- works
oneReadVar = var "a" $ \a -> var "b" $ \b -> loop b $ stream $ \b' -> do
 a <- readVar a
 async $ print a
 text a
 button [onClick] []
 pure $ Left ()

 -- readVar b. still loop, not about io or async
oneReadVarB = var "a" $ \a -> var "b" $ \b -> loop b $ stream $ \b' -> do
 b <- readVar b
 async $ print b
 text b
 button [onClick] []
 pure $ Left ()
-- "a"

textForever = do
  text "forever"

textAfterIO = do 
 io $ print "adbac"
 text "Abc"
-- now it works. StepIO should adavanceIO, not pure.


textInStream = var "a" $ \a -> var "b" $ \b -> loop b $ stream $ \b' -> do
  text "Abc"


onlyIO = do
  io $ print $ "only io"

textAfterAsync = do
  async $ print $ "only io"
  text "after"

-- async works. So it's io's problem.
