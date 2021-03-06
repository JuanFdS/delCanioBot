
module AppSpec where

import           Control.Exception (throwIO)
import           Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import           Network.HTTP.Types
import           Network.Wai (Application)
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Client
import           Test.Hspec
import           Data.List

import           App hiding (getCanio)

getCanio :: ClientM CanioDTO
raw :: Method -> ClientM Response
getCanio :<|> raw = client canioApi

spec :: Spec
spec = do
    describe "/canio" $ do
        withClient mkApp $ do
            it "devuelve una frase" $ \ env -> do
                try env getCanio >>= (`shouldSatisfy` (isPrefixOf "Nico Del Caño " . canio))


withClient :: IO Application -> SpecWith ClientEnv -> SpecWith ()
withClient x innerSpec =
    beforeAll (newManager defaultManagerSettings) $ do
    flip aroundWith innerSpec $ \ action httpManager -> do
        testWithApplication x $ \ port -> do
            let testBaseUrl = BaseUrl Http "localhost" port ""
            action (ClientEnv httpManager testBaseUrl Nothing)

type Host = (Manager, BaseUrl)

try :: ClientEnv -> ClientM a -> IO a
try clientEnv action = either throwIO return =<<
    runClientM action clientEnv