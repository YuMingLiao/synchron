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
-- import           Syn
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
import Concur (runConcur, withPool, step, orr')

counter x = do
  div [ onClick ] [ text (T.pack $ show x) ]
  counter (x + 1)

testConcurReplica = runConcur $ do
  orr' $ counter 0


testConcur :: IO ()
testConcur = Log.logger $ \log -> do
  v1 <- registerDelay 1000000
  v2 <- registerDelay 2000000
  v3 <- registerDelay 1500000
  v4 <- registerDelay 3000000
  v5 <- registerDelay 2500000

  (_, rs) <- runConcur $ do
    (_, rs) <- orr'
      [ dp log v3 "V3"
      , dp log v5 "V5"
      , do
          (_, rs) <- orr' [ dp log v1 "A", dp log v2 "B", dp log v4 "C" ]
          (_, rs) <- orr' rs
          (_, rs) <- orr' rs
          pure ()
      ]
    (_, rs) <- orr' rs
    orr' rs

  print $ length rs

  where
    dp log v s = do
      log ("BEFORE: " <> s)
      Concur.step $ do
        v' <- readTVar v
        check v'
      log ("AFTER: " <> s)

    f c n = do
      Concur.step $ writeTChan c (show n)
      f c (n + 1)

main = testConcur
