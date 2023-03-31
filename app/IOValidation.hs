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
import Control.Monad.IO.Class

counter x = do
  div [ onClick ] [ text (T.pack $ show x) ]
  counter (x + 1)

inputOnEnter p v = do
  e <- input [ autofocus True, placeholder p, value v, Left <$> onInput, Right <$> onKeyDown ]
  case e of
    Left e  -> inputOnEnter p (targetValue $ target e)
    Right e -> if kbdKey e == "Enter"
      then pure v
      else inputOnEnter p v

inputOnInput p v = do
  e <- input [ autofocus True, placeholder p, value v, onInput]
  pure . targetValue . target $ e

inputPassword p v = do
  e <- input [ autofocus True, placeholder p, type_ "password", value v, onInput]
  pure . targetValue . target $ e


 
shared :: Syn () ()
shared = local $ \end -> local $ \st -> pool $ \p -> do
  spawn p (set st end)
  spawn p (get st)
  await end
  where
    set st end = do
      emit st 4
      emit st 5
      emit st 6
      emit st 7
      emit end ()

    get st = do
      a <- await st
      async (print a)
      get st

ttext t = do
  span [ onDoubleClick ] [ text t ]
  inputOnEnter "" t
-- I guess putVar should never show in `\s -> ...`. Kind of violate something.
-- So I shouldn't use v to set up an initial value that can be changed by user later.
-- the putVar widget and showing widget needs to be separated.
-- so putVar should be in Syn. showing widget should be in loop stream.
-- a bit like parent widget keep control of shared state in Flutter.
--
-- Okay, so if I don't use spawn, it means squential showing widgets, one after another.
-- spawn is a bit like nailing a varying painting on the wall. It decides sequence I guess. Or is it just about event pool?
--  Syn.forever or Syn will end instantly. If there is a Syn here, the whole Syn will end with its branching logic.
--  spawn widget shows later than plain widget
--  spawns in andd shows horiztonal and reversed.
--  So no vertical for spawns.
--  But in todos, spawn div then vertical.
vert xs = div [] (map (\x -> div [] [x]) xs)
signUp = var "" $ \name -> var "" $ \pwd -> var "" $ \cfm -> pool $ \p -> do
  spawn p (div [] [inputName name])
  spawn p (div [] [validateName name])
  spawn p (div [] [inputPwd pwd])
  spawn p (div [] [validatePwd pwd])
  spawn p (div [] [inputCfm cfm])
  spawn p (div [] [validateCfm pwd cfm])
  -- vert [spawn p (validateName name), spawn p (inputName name)]
  Syn.forever
  where
    inputName v = go mempty
      where 
            go s = do
              s <- inputOnInput "請輸入姓名" s
              putVar v s
              go s 
    validateName v = loop v $ stream $ \s -> do
      text (T.pack $ show s)
    inputPwd v = go mempty
      where go s = do
              s <- inputPassword "請輸入密碼" s 
              putVar v s
              go s
    validatePwd v = loop v $ stream $ \s -> do
      text (T.pack $ show s)
    inputCfm v = go mempty
      where go s = do
              s <- inputPassword "請輸入密碼" s 
              putVar v s
              go s
    validateCfm vPwd vCfm = loop vPwd $ stream $ \sPwd -> loop vCfm $ stream $ \sCfm -> do
      text (T.pack $ show (sPwd == sCfm))
 
main = runReplica signUp 
