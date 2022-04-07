{-# OPTIONS_GHC -O2 -fno-warn-missing-fields #-}
{-# LANGUAGE RecordWildCards                 #-}

module Traumatic
  ( traumatic
  ) where

import Network.HTTP.Client (Proxy(..))

import Data.Char (isDigit)

import Engine
import Captcha
import Init
import Static

import Prelude hiding (pairs)

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
performSinglePost captcha_meta' post = do
    let
      captcha_meta =
        captcha_meta' { captcha_proxy = (post_proxy post) }

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
      Just resp ->
        resp

init_posts :: Config -> IO [Post]
init_posts Config{..} = do
    case proxy_mode params of
      NoProxy ->
        buildSinglePost params static
            >>= pure . (: [])
      WithProxy ->
        mapM (\p -> (add_proxy p) <$> buildSinglePost params static)
            $ take (min (length pr) (threads_count params)) pr
                where pr = proxies static

{-# INLINE sendSingle #-}
sendSingle captcha_meta post = do
    resp <- performSinglePost captcha_meta post
    putStrLn . show $ resp
    pure resp

-- makaba response codes:
{-# INLINE badCodes #-}
badCodes = [
        404 -- general fail code
    ]

{-# INLINE checkBanned #-}
checkBanned MakabaResponse{..} =
    maybe Nothing (\p -> if err_code `elem` badCodes then Just p else Nothing)
        current_proxy

-- init posts, also overwrite config to filter bad proxies.
main_init :: Config -> IO Config
main_init conf@Config{..} = do
    let
      captcha_meta = 
        CaptchaMeta (anti_captcha_type params) (anti_captcha_key params) Nothing
    posts        <- init_posts conf
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
      NoProxy ->
        pure ()
      WithProxy ->
        putStrLn $ "[filter] плохих проксей: " <> (show . length $ banned)
    pure $
        conf { static = new_static }

main_loop :: Config -> IO ()
main_loop conf = do
    if (<= 0) . times_count . params $ conf
      then die "[quit] завершено."
      else pure ()

    new_conf <- main_init conf -- init and post.

    if (null . proxies . static $ new_conf) && (proxy_mode . params $ conf) == WithProxy 
      then die "[quit] все проксичи умерли, помянем."
      else do
        let
          base_delay =
            delay_count . params $ conf
        secs <- randomRIO (base_delay, base_delay + 3)
        threadDelay $ secs * 1000000

        let
          new_params =
            (params conf) { times_count = (\x -> x - 1) . times_count . params $ conf }

        main_loop $
            new_conf { params = new_params }

{-# INLINE traumatic #-}
traumatic :: Static -> InitParams -> IO ()
traumatic static params = do
    putStrLn . show $ params
    main_loop $ Config params static 

