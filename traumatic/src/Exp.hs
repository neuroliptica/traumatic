{-# LANGUAGE RecordWildCards #-}

-- 
-- | Экспериментальный вариант Traumatic.hs, переписанный с MTL.
-- | Текущий Traumatic.hs пока just works. Этот вариант нужен для проверки всяких идей.
-- | Целью является сокращение быдлокода и бойлерплейта путём использования Reader и State монад.
--

module Exp
  where

import Network.HTTP.Client (Proxy(..))

import Data.Char (isDigit)

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

data Env = Env InitParams Static
    deriving Show

type ThreadNum = String

--data Unit = Unit CaptchaMeta Post
--    deriving Show

getCaptions (Env _ Static {..}) = captions
getPictures (Env _ Static {..}) = pictures
getProxies (Env _ Static {..}) = proxies

isNeedAppendPics (Env InitParams {..} _) = append_pic
isNeedAppendSage (Env InitParams {..} _) = append_sage

getBoard (Env InitParams {..} _) = init_board
getThread (Env InitParams {..} _) = init_thread

getWipeMode (Env InitParams {..} _) = wipe_mode
getProxyMode (Env InitParams {..} _) = proxy_mode

getThreadsCount (Env InitParams {..} _) = threads_count

createPost :: Reader Env (IO Post)
createPost = do
    texts <- asks getCaptions
    files <- asks getPictures
    mode  <- asks getWipeMode

    needSage <- asks isNeedAppendSage
    needFile <- asks isNeedAppendPics

    board  <- asks getBoard
    thread <- asks getThread

    return $ do
        pic <- if needFile && length files > 0
                then get_random files >>= pure . Just
                else pure Nothing
        text' <- get_random texts

        case mode of
          SingleThread ->
            maybe (error "Ошибка инициализации.")
                  (\t -> pure $ Post (PostMeta board t) Nothing text' pic needSage)
                  thread
          Shrapnel -> do
            t <- getRandomThreadE board
            pure $ Post (PostMeta board t) Nothing text' pic needSage

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


