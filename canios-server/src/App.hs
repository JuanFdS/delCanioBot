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
  "canio" :> Get '[JSON] CanioDTO
  :<|> Raw

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
  :<|> serveDirectoryWebApp "canios-server/static"

getCanio :: Handler CanioDTO
getCanio = liftIO $ CanioDTO <$> fmap unaFrase generadorCanio

newtype Canio = Canio String deriving (Eq, Show, Generic)

canio :: CanioDTO -> String
canio (CanioDTO str) = str

newtype CanioDTO = CanioDTO {
      frase:: String
    } deriving (Generic, Show)

instance ToJSON CanioDTO
instance FromJSON CanioDTO