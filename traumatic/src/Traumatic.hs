{-# OPTIONS_GHC -O2          #-}
{-# LANGUAGE RecordWildCards #-}

module Traumatic
  (
    traumatic
  , buildStatic
  , Static(..)
  ) where

import Network.HTTP.Client

import qualified Data.ByteString.Lazy.Internal as LBS
import qualified Data.ByteString.Internal      as BS

import Data.Char (isDigit)

import Config
import Engine
import Captcha
import Init

import Prelude hiding (pairs)
import Control.Monad

import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent       (threadDelay)

-- proxy utils.
{-# INLINE proxy_pair #-}
proxy_pair :: String -> (String, String)
proxy_pair xs = let (port, ip) = break (\x -> x == ':') . reverse $ xs
                in (reverse . tail $ ip, reverse port)

{-# INLINE proxy_filter #-}
proxy_filter :: (String, String) -> Maybe (String, Int)
proxy_filter (ip, port) =
    if all isDigit port
      then Just (ip, read port)
      else Nothing

{-# INLINE add_proxy #-}
add_proxy :: Proxy -> Post -> Post
add_proxy proxy' post' = post' { post_proxy = Just proxy' }

-- general static data initialization.
data Static = Static
  { proxies  :: ![Proxy] 
  , captions :: ![FilePath] 
  , pictures :: ![FilePath] }
  deriving Show

toStatic :: Config -> Static 
toStatic Config{..} = Static proxy' caption_list pics
    where pairs = filter (not . null) . map (proxy_filter . proxy_pair) $ proxy_list
          Just proxy' = map (\(ip, port) -> Proxy (BS.packChars ip) port) <$> sequence pairs
          pics = map (_pics <>) pics_list

buildStatic :: IO (Maybe Static)
buildStatic = do
    config <- initConfig
    let static_config = toStatic <$> config
    case static_config of
      Left msg -> do
        putStrLn ("[init] Eггог, " <> msg) >> pure Nothing
      Right static' -> do
        putStrLn "[init] Ok, начальные данные успешно инициализированы."
        pure . Just $ static'

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
             , post_sage  = append_sage }
    case wipe_mode of
      SingleThread ->
        (\(Just thread') ->
            pure $ post' { meta = PostMeta init_board thread' })
                $ init_thread
      Shrapnel -> do
        catalog <- getThreads init_board
        case catalog of
          Nothing -> error "не удалось получить каталог тредов!"
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
    maybe (pure $ 404 `with` "ошибка получения капчи.") id $ response

-- posts initialisation.
init_posts :: Static -> InitParams -> IO [Post]
init_posts static params = do
    case proxy_mode params of
      NoProxy ->
        buildSinglePost params static
            >>= pure . (:[])
      WithProxy ->
        mapM (\p -> (add_proxy p) <$> buildSinglePost params static)
                $ (proxies static)

main_init :: Static -> InitParams -> IO ()
main_init static params = do
    posts <- init_posts static params
    let
      posts_to_send =
        (threads_count params) * length posts
    let
      captcha_meta =
        CaptchaMeta (anti_captcha_type params) (anti_captcha_key params)

    replicateM_ (threads_count params)
        $ do
            mapConcurrently
                (\post -> do
                    response <- performSinglePost captcha_meta post
                    putStrLn . show $ response) $ posts
            putStrLn "[main_thread] sleep for 5 secs."
            threadDelay 5000000
    putStrLn "[finished]"

{-# INLINE traumatic #-}
traumatic :: Static -> InitParams -> IO ()
traumatic static params = (putStrLn . show $ params) >> main_init static params 

