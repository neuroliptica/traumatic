{-# OPTIONS_GHC -O2          #-}
{-# LANGUAGE RecordWildCards #-}

module Traumatic
  ( traumatic
  ) where

import Network.HTTP.Client (Proxy(..))

import Engine
import Captcha
import Init
import Static

import Prelude hiding (pairs)

import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent       (threadDelay)

import Control.Monad.Reader

import System.Random (randomRIO)
import System.Exit   (die)

{-# INLINE add_proxy #-}
add_proxy :: Proxy -> Post -> Post
add_proxy proxy' post' = post' { post_proxy = Just proxy' }

badCodes = [
        404 -- general fail code.
    ]

data Env = Env InitParams Static
    deriving Show

type ThreadNum = String

getCaptions (Env _ Static {..}) = captions
getPictures (Env _ Static {..}) = pictures
getProxies (Env _ Static {..}) = proxies

getPicsCount (Env InitParams {..} _) = pics_count
isNeedAppendSage (Env InitParams {..} _) = append_sage

getBoard (Env InitParams {..} _) = init_board
getThread (Env InitParams {..} _) = init_thread

getWipeMode (Env InitParams {..} _) = wipe_mode
getProxyMode (Env InitParams {..} _) = proxy_mode

getThreadsCount (Env InitParams {..} _) = threads_count

getAntiCaptchaKey (Env InitParams {..} _) = anti_captcha_key
getAntiCaptchaType (Env InitParams {..} _) = anti_captcha_type

getTimesCount (Env InitParams {..} _) = times_count
getDelayCount (Env InitParams {..} _) = delay_count

createPost :: Reader Env (IO Post)
createPost = do
    texts <- asks getCaptions
    files <- asks getPictures
    mode  <- asks getWipeMode

    needSage <- asks isNeedAppendSage
    filesC <- asks getPicsCount

    board  <- asks getBoard
    thread <- asks getThread

    return $ do
        pics <- replicateM filesC (get_random files) 
        text' <- get_random texts

        case mode of
          SingleThread ->
            maybe (error "Ошибка инициализации.")
                  (\t -> pure $ Post (PostMeta board t) Nothing text' pics needSage)
                  thread
          Shrapnel -> do
            t <- getRandomThreadE board
            pure $ Post (PostMeta board t) Nothing text' pics needSage

getRandomThreadE :: String -> IO ThreadNum
getRandomThreadE board = do
    catalog <- getThreads board
    case catalog of
      Nothing -> die "Фатальная ошибка. Не удалось получить каталог тредов."
      Just catalog' -> get_random $ _threads catalog'
                       >>= pure . _num

createPosts :: Reader Env (IO [Post])
createPosts = do
    proxyMode <- asks getProxyMode
    config <- ask
    case proxyMode of
      NoProxy -> return $ do
        single <- runReader createPost config
        pure [single]
      WithProxy -> do
        proxies <- asks getProxies
        times <- asks getThreadsCount >>= pure . min (length proxies)
        
        return . mapM (\p -> add_proxy p <$> runReader createPost config)
                      $ take times proxies

performPostR :: Post -> Reader Env (IO MakabaResponse)
performPostR post = do
    antiCaptchaKey <- asks getAntiCaptchaKey
    antiCaptchaType <- asks getAntiCaptchaType 

    let captchaMeta = CaptchaMeta antiCaptchaType
                                  antiCaptchaKey 
                                  (post_proxy post)
    return $ do
        putStrLn $ show captchaMeta <> "получение капчи..."
        captcha <- getCaptcha captchaMeta

        let response = do
                postEnv <- PostEnv post <$> captcha
                return $ runReader performPost postEnv

        let with = MakabaResponse (post_proxy post)
        case response of
            Nothing -> pure $ 404 `with` "ошибка получения капчи."
            Just makabaResponse -> makabaResponse

{-# INLINE performSingle #-}
performSingle :: Post -> Reader Env (IO MakabaResponse)
performSingle post = do
    config <- ask
    return $ do
        response <- runReader (performPostR post) config
        putStrLn . show $ response
        pure response

{-# INLINE isProxyBanned #-}
isProxyBanned MakabaResponse{..} = err_code `elem` badCodes

{-# INLINE filterBanned #-}
filterBanned :: Static -> [Proxy] -> Static
filterBanned old bad =
    let good = filter (not . (`elem` bad)) (proxies old)
    in old { proxies = good } 

performPosts :: Env -> IO Env
performPosts env@(Env pars stat) = do
    posts <- runReader createPosts env
    let postsR = map performSingle posts
    responses <- mapConcurrently (\r -> runReader r env) postsR

    let (Just banned) = sequence . map current_proxy . filter isProxyBanned
                        $ responses
    let newStatic = filterBanned stat banned

    putStrLn $ "[filter] плохих проксей: " <> (show . length $ banned)
    return $ Env pars newStatic

mainLoop :: Reader Env (IO ())
mainLoop = do
    times     <- asks getTimesCount
    delayTime <- asks getDelayCount
    env       <- ask

    let modifiedEnv = do
            if times <= 0
                then die "[quit] завершено."
                else pure ()

            nenv@(Env pars stat) <- performPosts env

            if (getProxyMode nenv == WithProxy) && (null . getProxies $ nenv)
                then die "[quit] все проксичи умерли, помянем."
                else do
                    secs <- randomRIO (delayTime, delayTime + 3)
                    threadDelay $ secs * 1000000
                    let newPars = pars { times_count = times - 1 }

                    pure (Env newPars stat)
    return $ do
        menv <- modifiedEnv
        runReader mainLoop menv

{-# INLINE traumatic #-}
traumatic :: Static -> InitParams -> IO ()
traumatic static params = do
    putStrLn . show $ params
    runReader mainLoop $ Env params static

