{-# LANGUAGE OverloadedStrings #-}

module Main where

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
import           Syn hiding (pool, spawn, orr)
-- import           Syn.SVG.Replica

import           Var

import Network.HTTP.Types.Status
import Network.WebSockets.Connection
import Network.Wai

import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.Replica as Replica

import Prelude hiding (div, forever, span)

import Debug.Trace
import System.IO.Unsafe
import Concur (runConcur, withPool, spawn, orr)


-- testConnectors :: IO ()
testConnectors = do
  HTTP.http 3921 $ \http ->
    void $ WS.websocket 3922 defaultConnectionOptions $ \wss ->
    WS.websocket 3923 defaultConnectionOptions $ \wss2 ->
    Log.logger $ \log -> do

      runConcur $ auth http log wss wss2

    where
      auth http log wss wss2 = do
        r <- HTTP.receive http $ \req respond -> do
          r <- respond $ responseLBS status200 [] "Hello World"
          pure (r, "good")
        log r
        server log wss wss2
      
      server log wss wss2 = withPool $ \pool -> Control.Monad.forever $ do
        [ws, ws2] <- andd [ WS.accept wss, WS.accept wss2 ]
        spawn pool (go log ws ws2)
        
      go log ws ws2 = do
        r <- orr
          [ fmap Left  <$> WS.receive ws
          , fmap Right <$> WS.receive ws2
          ]
        case r of
          Nothing  -> pure ()
          _  -> do
            log $ show r
            go log ws ws2

