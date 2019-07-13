{-# LANGUAGE OverloadedStrings #-}

module Twitter(post) where

import Web.Twitter.Conduit hiding (map)

import Control.Applicative
import Data.Monoid
import System.Environment
import Configuration.Dotenv
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Char8 as S8

schemaFile = ".scheme.yml"

post :: String -> IO ()
post status = do
    putStrLn $ "Post message: " <> status
    twInfo <- getTWInfoFromEnv
    mgr <- newManager tlsManagerSettings
    res <- call twInfo mgr $ update $ T.pack status
    print res


getTWInfoFromEnv :: IO TWInfo
getTWInfoFromEnv = do
    (oa, cred) <- getOAuthTokens
    return $ setCredential oa cred def

getOAuthTokens :: IO (OAuth, Credential)
getOAuthTokens = do
    loadSafeFile defaultValidatorMap schemaFile defaultConfig
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