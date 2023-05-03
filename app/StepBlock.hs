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

-- Single StepBlock effect. tryPeek gets value.
singleEffect = do
  runReplica $ do
   str <- effect (do threadDelay 5000000; pure "Hello";)
   button [onClick] [text str]
   text str

-- orr single effect. works.
orrSingleEffect = do
  runReplica $ do
   s <- orr [effect (do threadDelay 3000000; pure "Hello";)]
   text s

-- orr two effects. works.
orrTwoEffect = do
  runReplica $ do
   s <- orr [effect (do threadDelay 4000000; pure "Hello";), effect (do threadDelay 3000000; pure "World";)]
   text s

orrOneViewOneEffect = do
  runReplica $ do
   s <- orr [effect (do threadDelay 3000000; pure "Hello";), text "effect or text"]
   text s

-- both way works.
orrOneButtonOneEffect = do
  runReplica $ do
   s <- orr [effect (do threadDelay 3000000; pure "timeout";), "clicked" <$ button [onClick] [text "click"]]
   text s


main = orrOneButtonOneEffect
