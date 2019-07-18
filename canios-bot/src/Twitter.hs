{-# LANGUAGE OverloadedStrings #-}

module Twitter(post) where

import Web.Twitter.Conduit hiding (map)

import Control.Applicative
import Data.Monoid
import System.Environment
import Configuration.Dotenv
import qualified Network.HTTP.Client as HTTP
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Char8 as S8
import Canios

post :: [Canio] -> IO ()
post canios = do
    let status = unaFrase canios
    twInfo <- getTWInfoFromEnv
    mgr <- newManager tlsManagerSettings
    bs <- unaImagen canios
    call twInfo mgr $ updateWithMedia (T.pack status) (MediaRequestBody "delCanio" (HTTP.RequestBodyBS bs))



getTWInfoFromEnv :: IO TWInfo
getTWInfoFromEnv = do
    loadFile defaultConfig
    (oa, cred) <- getOAuthTokens
    return $ setCredential oa cred def

getOAuthTokens :: IO (OAuth, Credential)
getOAuthTokens = do
    consumerKey <- getEnv' "TWITTER_API_KEY"
    consumerSecret <- getEnv' "TWITTER_API_SECRET"
    accessToken <- getEnv' "TWITTER_OAUTH_TOKEN"
    accessSecret <- getEnv' "TWITTER_OAUTH_SECRET"
    let oauth = twitterOAuth
            { oauthConsumerKey = consumerKey
            , oauthConsumerSecret = consumerSecret
            }
        cred = Credential
            [ ("oauth_token", accessToken)
            , ("oauth_token_secret", accessSecret)
            ]
    return (oauth, cred)
  where 
        getEnv' = (S8.pack <$>) . getEnv