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

main = testOrr
testOrr = do
  runReplica $ do
    -- effect (do threadDelay 5000000; pure 1;)
    -- button [onClick] []
    --orr [button [onClick] [text "button"], text "text"]
    -- orr [text "text", button [onClick] [text "button"]]
    -- orr [text "text", button [onClick] []]
    button [onClick] [text "preview"]
    -- orr [text "text", effect (do threadDelay 10000000; pure 1;)]
    orr [2 <$ button [onClick] [text "text"], effect (do threadDelay 10000000; pure 1;)]
    -- effect (do threadDelay 1000000; pure 1;)
    text "orr returned"

-- bug: halt, no io returns.
