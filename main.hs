{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Web.Twitter.Conduit

import Data.Aeson
import Web.Authenticate.OAuth as OA
import qualified Network.URI as URI
import Network.HTTP.Conduit
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as S8
import qualified Data.CaseInsensitive as CI
import Data.Conduit
import qualified Data.Conduit.List as CL
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Base
import System.Environment
import Control.Monad.Logger
import Control.Lens
import Data.Ord (comparing)
import System.FilePath ((</>))
import qualified System.FilePath as FilePath
import qualified Data.Conduit.Binary as CB
import System.IO (withFile, IOMode(..))

getOAuthTokens :: IO (OAuth, Credential)
getOAuthTokens = do
    consumerKey <- getEnv' "OAUTH_CONSUMER_KEY"
    consumerSecret <- getEnv' "OAUTH_CONSUMER_SECRET"
    accessToken <- getEnv' "OAUTH_ACCESS_TOKEN"
    accessSecret <- getEnv' "OAUTH_ACCESS_SECRET"
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

getProxyEnv :: IO (Maybe Proxy)
getProxyEnv = do
    env <- M.fromList . over (mapped . _1) CI.mk <$> getEnvironment
    let u = M.lookup "https_proxy" env <|>
            M.lookup "http_proxy" env <|>
            M.lookup "proxy" env >>= URI.parseURI >>= URI.uriAuthority
    return $ Proxy <$> (S8.pack . URI.uriRegName <$> u) <*> (parsePort . URI.uriPort <$> u)
  where
    parsePort :: String -> Int
    parsePort []       = 8080
    parsePort (':':xs) = read xs
    parsePort xs       = error $ "port number parse failed " ++ xs

runTwitterFromEnv :: (MonadIO m, MonadBaseControl IO m) => TW (ResourceT m) a -> m a
runTwitterFromEnv task = do
    pr <- liftBase getProxyEnv
    (oa, cred) <- liftBase getOAuthTokens
    let env = (setCredential oa cred def) { twProxy = pr }
    runTW env task

runTwitterFromEnv' :: (MonadIO m, MonadBaseControl IO m) => TW (ResourceT (NoLoggingT m)) a -> m a
runTwitterFromEnv' = runNoLoggingT . runTwitterFromEnv

imageInfo :: Status -> [(String, String)]
imageInfo st = mediaList^..traversed.to mkInfo._Just
  where
    mediaList = st^..statusEntities._Just.enMedia.traversed.entityBody

    mkInfo :: MediaEntity -> Maybe (String, String)
    mkInfo ent = case imageSizeMax ent of
        Just (size, _) -> Just (T.unpack $ ent^.meMediaURL, T.unpack size)
        Nothing -> Nothing

imageSizeMax :: MediaEntity -> Maybe (T.Text, MediaSize)
imageSizeMax = maximumByOf (meSizes.itraversed.withIndex) (\(_, a) (_, b) -> cmpMediaSizes a b)
  where
    cmpMediaSizes = comparing (\x -> (x^.msWidth, x^.msHeight))

fetchImage :: TwitterBaseM m => FilePath -> Status -> TW m ()
fetchImage dir st = do
    let sid = show $ st^.statusId
    liftIO . putStrLn $ "Fetching image: " ++ sid
    rawjson <- call' . showId $ st^. statusId
    liftIO $ withFile (sid ++ ".json") WriteMode $ \hndl -> L8.hPutStrLn hndl $ encode (rawjson :: Value)
    liftIO . withManager $ \mgr -> do
        iforM_ (imageInfo st) $ \i (url, size) -> do
            liftIO . putStrLn $ "Fetching each image: " ++ show (sid, url, size)
            let ext = FilePath.takeExtension url
            req <- liftIO $ parseUrl (url ++ ":" ++ size)
            res <- http req mgr
            responseBody res $$+- CB.sinkFile (dir </> sid ++ "_" ++ show i ++ "_" ++ size ++ ext)

handleEvent :: TwitterBaseM m => StreamingAPI -> TW m ()
handleEvent (SEvent event)
    | event^.evEvent == "favorite",
      Just (ETStatus st) <- (event ^. evTargetObject) = do
          fetchImage "./" st
handleEvent _ = return ()

main :: IO ()
main = runTwitterFromEnv' $ do
    st <- stream userstream
    st $$+- CL.mapM_ handleEvent
