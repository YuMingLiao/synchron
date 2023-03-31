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

data SignUpForm = SignUpForm {
  username :: Text,
  password :: Text,
  confirmPassword :: Text
} deriving (Generic, Show)

defaultSignUp = SignUpForm "" "" ""

type Validation a = HKD a (Const Bool)

instance Semigroup Bool where
  (<>) = (&&)

instance Monoid Bool where
  mempty = False

example = print $ (deconstruct defaultSignUp :: Validation SignUpForm) & field @"username" .~ Const True 
egSyn = do
  io $ example
{-
egVar = var (deconstruct defaultSignUp :: Validation SignUpForm) $ \v -> do
  v <- readVar v
  io $ print v

egVarField = var (deconstruct defaultSignUp :: Validation SignUpForm) $ \v -> do
  v <- readVar v
  io $ print (v & field @"username" .~ Const True)
-}

egVarPutVar = var (deconstruct defaultSignUp :: Validation SignUpForm) $ \v -> do
  s <- readVar v
  -- io $ print (s & field @"username" .~ Const True)
  putVar v (s & field @"username" .~ Const True)


main = runReplica egVarPutVar

