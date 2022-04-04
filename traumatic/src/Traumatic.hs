{-# OPTIONS_GHC -O2          #-}
{-# LANGUAGE RecordWildCards #-}

module Traumatic
  ( traumatic
  ) where

import Network.HTTP.Client

import qualified Data.ByteString.Lazy.Internal as LBS
import qualified Data.ByteString.Internal      as BS

import Data.Char (isDigit)

import Engine
import Captcha
import Init
import Static

import Prelude hiding (pairs)
import Control.Monad

import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent       (threadDelay)

import System.Random (randomRIO)
import System.Exit   (die)

-- proxy utils.
{-# INLINE add_proxy #-}
add_proxy :: Proxy -> Post -> Post
add_proxy proxy' post' = post' { post_proxy = Just proxy' }

data Config = Config
  { params  :: InitParams
  , static  :: Static }
  deriving Show

-- general single post builing.
buildSinglePost :: InitParams -> Static -> IO Post
buildSinglePost InitParams{..} Static{..} = do
    text' <- get_random captions
    file' <- if length pictures > 0 && append_pic
                then
                  get_random pictures >>= pure . Just
                else
                  pure Nothing 
    let
      post' =
        Post { text = text' 
             , file = file'
             , post_proxy = Nothing
             , post_sage  = append_sage
             }
    case wipe_mode of
      SingleThread ->
        (\(Just thread') ->
            pure $ post' { meta = PostMeta init_board thread' })
                $ init_thread
      Shrapnel -> do
        catalog <- getThreads init_board
        case catalog of
          Nothing -> die "Фатальная ошибка. Не удалось получить каталог тредов."
          Just cat -> get_random $ _threads cat
                        >>= pure . (\t -> post' { meta = PostMeta init_board (_num t) })

-- performing.
performSinglePost :: CaptchaMeta -> Post -> IO MakabaResponse
performSinglePost captcha_meta post = do
    captcha <- getCaptcha captcha_meta
    let
      response =
        performPost post <$> captcha
    let
      with =
        MakabaResponse (post_proxy post)

    case response of
      Nothing ->
        pure $ 404 `with` "ошибка получения капчи."
      Just resp -> resp

init_posts :: Int -> Config -> IO [Post]
init_posts count Config{..} = do
    case proxy_mode params of
      NoProxy ->
        buildSinglePost params static
            >>= pure . (: [])
      WithProxy ->
        mapM (\p -> (add_proxy p) <$> buildSinglePost params static)
            $ take (min (length pr) count) pr
                where pr = proxies static

{-# INLINE sendSingle #-}
sendSingle captcha_meta post = do
    resp <- performSinglePost captcha_meta post
    putStrLn . show $ resp
    pure resp

{-# INLINE checkBanned #-}
checkBanned MakabaResponse{..} =
    case current_proxy of
      Nothing -> Nothing
      Just prx -> if err_code == 404 then Just prx else Nothing

main_init :: Config -> IO Config
main_init conf@Config{..} = do
    let
      captcha_meta = 
        CaptchaMeta (anti_captcha_type params) (anti_captcha_key params)
    posts        <- init_posts 10 conf -- 10 threads hardcoded.
    bad_response <-
        (map checkBanned) <$> mapConcurrently (sendSingle captcha_meta) posts
    let 
      (Just banned) = 
        sequence . filter (not . null) $ bad_response
    let
      good =
        filter (not . (`elem` banned)) (proxies static)
    let
      new_static =
        Static good (captions static) (pictures static)

    case proxy_mode params of
      NoProxy -> pure ()
      WithProxy -> putStrLn $ "[filter] плохих проксей: " <> (show . length $ banned)

    pure $ conf { static = new_static }

main_loop :: Config -> IO ()
main_loop conf = do
    new_conf <- main_init conf
    if (proxy_mode . params $ conf) == WithProxy && (null . proxies . static $ new_conf)
      then die "[quit] все проксичи умерли, помянем."
      else main_loop new_conf 

-- posts initialisation.
{-
init_posts :: Config -> IO [Post]
init_posts Config{..} = do
    case proxy_mode params of
      NoProxy ->
        buildSinglePost params static
            >>= pure . (:[])
      WithProxy ->
        mapM (\p -> (add_proxy p) <$> buildSinglePost params static)
                $ (proxies static)

main_init :: Config -> IO ()
main_init conf@Config{..} = do
    let
      captcha_meta =
        CaptchaMeta (anti_captcha_type params) (anti_captcha_key params)

    replicateM_ (threads_count params)
        $ do
            posts <- init_posts conf -- building posts
            mapConcurrently          -- then launch them concurrently
                (\post -> do
                    response <- performSinglePost captcha_meta post
                    putStrLn . show $ response)
                        $ posts

            secs <- randomRIO (1, 7)
            threadDelay $ secs * 1000000
-}

{-# INLINE traumatic #-}
traumatic :: Static -> InitParams -> IO ()
traumatic static params = do
    putStrLn . show $ params
    main_loop $ Config params static 

