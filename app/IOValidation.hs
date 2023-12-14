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
import Data.Maybe (catMaybes)

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


 
vert xs = div [] (map (\x -> div [] [x]) xs)

data SignUpForm = SignUpForm {
  username :: Text,
  password :: Text,
  confirmPassword :: Text
} deriving (Show, Generic)

defaultSignUp = SignUpForm "" "" ""

type Validation a = HKD a (Const Bool)


instance Semigroup Bool where
  (<>) = (&&)

instance Monoid Bool where
  mempty = False


signUp = var "" $ \name -> var "" $ \pwd -> var "" $ \cfm -> pool $ \p -> do
  spawn p (div [] [inputName name])
  spawn p (div [] [errName name])
  spawn p (div [] [inputPwd pwd])
  spawn p (div [] [errPwd pwd ])
  spawn p (div [] [inputCfm cfm])
  spawn p (div [] [errCfm pwd cfm])
  spawn p $ submitButton name pwd cfm 
  Syn.forever
  where
    inputName v = go mempty
      where 
            go s = do
              s <- inputOnInput "請輸入姓名" s
              putVar v s
              go s 
    note True n = Nothing
    note False n = Just n
    vert xs = div [] (map (\x -> div [] [x]) xs)
    validateName s = do
      sequence [ print "io validation" >> pure ((s /= "") `note` "Name cannot be empty.")
               , pure $ True `note` "This name have been taken."
               ]
    errName v = loop v $ stream $ \s -> do
      errors <- io $ catMaybes <$> validateName s
      vert (map text errors)
    inputPwd v = go mempty
      where go s = do
              s <- inputPassword "請輸入密碼" s 
              putVar v s
              go s
    validatePwd s = 
      [ (s /= "") `note` "Password cannot be empty." ] 
    errPwd v = loop v $ stream $ \s -> do
      vert (map text (catMaybes (validatePwd s))) 
    inputCfm v = go mempty
      where go s = do
              s <- inputPassword "請確認密碼" s 
              putVar v s
              go s
    validateCfm p c = 
      [ (p == c) `note` "Passwords do not match." ] 
    errCfm p c = loop p $ stream $ \p -> loop c $ stream $ \c -> do
      vert (map text (catMaybes (validateCfm p c))) 
    submitButton n p c = loop n $ stream $ \n -> loop p $ stream $ \p -> loop c $ stream $ \c -> do 
      nResults <- io $ validateName n
      case catMaybes (nResults ++ validatePwd p ++ validateCfm p c) of
           [] -> do 
             button [onClick] [text "Submit"]
             pure (Left (Left (Left ())))
           _ -> button [] [text "Can't Submit"]
                                   
 
main = runReplica signUp 
