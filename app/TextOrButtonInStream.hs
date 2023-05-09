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
import Data.Text (Text, pack)
import Control.Lens ((.~), (^.), (&), Const (..), Identity, anyOf)

string = text . pack . show
main = runReplica spawnButtonInStream 

-- works
textInStream = pool $ \p -> var "a" $ \v -> do
  spawn p $ changeString v
  showString v
  where
    showString v = loop v $ stream $ \s -> do
      string s
    changeString v = do
      button [onClick] [text "click"]
      putVar v "b"

-- works
buttonInStream = pool $ \p -> var "a" $ \v -> do
  spawn p $ changeString v
  showString v
  where
    showString v = loop v $ stream $ \s -> do
       button [Left () <$ onClick] [string s]
    changeString v = do
      button [onClick] [text "click"]
      putVar v "b"

-- works 
spawnButtonInStream = pool $ \p -> var "a" $ \v -> do
  spawn p $ changeString v
  spawn p $ showString v
  Syn.forever
  where
    showString v = loop v $ stream $ \s -> do
       button [Left () <$ onClick] [string s]
    changeString v = do
      button [onClick] [text "click"]
      putVar v "b"

twoLoopsWithSameVar = pool $ \p -> var "a" $ \v -> do
  spawn p $ changeString v
  spawn p $ showString v
  Syn.forever
  where
    showString v = loop v $ stream $ \s -> do
       button [Left () <$ onClick] [string s]
    showString v = loop v $ stream $ \s -> do
       button [Left () <$ onClick] [string s]
 
    changeString v = do
      button [onClick] [text "click"]
      putVar v "b"
