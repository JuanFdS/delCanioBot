{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module App where

import           Data.Aeson
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.IO
import           Canios
import           Control.Monad.IO.Class
import           System.Environment

-- * api

type CanioApi =
  "canio" :> Get '[JSON] Canio

canioApi :: Proxy CanioApi
canioApi = Proxy

-- * app

run :: IO ()
run = do
  port <- maybe 8080 read <$> lookupEnv "PORT"
  let settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("Escuchando en el puerto " ++ show port))
        defaultSettings
  runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = return $ serve canioApi server

server :: Server CanioApi
server =
  getCanio

getCanio :: Handler Canio
getCanio = Canio <$> liftIO generadorCanio

newtype Canio = Canio String deriving (Eq, Show, Generic)

canio :: Canio -> String
canio (Canio str) = str

instance ToJSON Canio
instance FromJSON Canio