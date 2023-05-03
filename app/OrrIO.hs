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

testOrr = do
  runReplica $ do
    -- effect (do threadDelay 5000000; pure 1;)
    -- button [onClick] []
    --orr [button [onClick] [text "button"], text "text"]
    -- orr [text "text", button [onClick] [text "button"]]
    -- orr [text "text", button [onClick] []]
    -- button [onClick] [text "preview"]
    -- orr [text "text", effect (do threadDelay 10000000; pure 1;)]
    s <- orr ["clicked" <$ button [onClick] [text "click"], effect (do threadDelay 2000000; pure "timeout";)]
    -- effect (do threadDelay 1000000; pure 1;)
    text s

testButtonBeforeOrr = do
  runReplica $ do
    button [onClick] [text "view"]
    s <- orr ["clicked" <$ button [onClick] [text "click"], effect (do threadDelay 2000000; pure "timeout";)]
    text s

-- works
testTwoButtons = do
  runReplica $ do
    button [onClick] [text "button 1"]
    button [onClick] [text "button 2"]
    text "You've clicked two buttons"

-- works
-- session is unlrated.
testOrrs = do
  runReplica $ do
   orr ["clicked" <$ button [onClick] [text "1"], effect (do threadDelay 2000000; pure "timeout";)]
   orr ["clicked" <$ button [onClick] [text "2"], effect (do threadDelay 2000000; pure "timeout";)]
   orr ["clicked" <$ button [onClick] [text "3"], effect (do threadDelay 2000000; pure "timeout";)]
   orr ["clicked" <$ button [onClick] [text "4"], effect (do threadDelay 2000000; pure "timeout";)]
   s <- orr ["clicked" <$ button [onClick] [text "5"], effect (do threadDelay 2000000; pure "timeout";)]
   text s

-- now working. so orr is unrelated.
-- focusing on effect
testButtonBeforeEffect = do
  runReplica $ do
    button [onClick] [text "button 1"]
    effect (do threadDelay 5000000; pure 1;)
    text "You've clicked 1 button"

testEffect = do
  runReplica $ do
    effect (do threadDelay 5000000; pure 1;)
    text "E end"

testEEBE = do
 runReplica $ do
    effect (do threadDelay 2000000;)
    effect (do threadDelay 2000000;)
    button [onClick] [text "Button"]
    effect (do threadDelay 2000000;)
    text "EEBE end"



main = testEEBE
