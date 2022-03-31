{-# OPTIONS_GHC -O2          #-}
{-# LANGUAGE RecordWildCards #-}

module Static
  ( Static(..)
  , buildStatic 
  ) where

import Network.HTTP.Client (Proxy(..))

import qualified Data.ByteString as BS (readFile)
import Data.ByteString.Internal        (packChars, unpackChars)

import Data.List.Split (splitOn)
import Data.List       (isSuffixOf)
import Data.Char       (isDigit)

import System.Directory (doesFileExist, doesDirectoryExist, getDirectoryContents)

import Prelude hiding (init, pairs)

data Init = Init
  { proxy_list   :: ![String]
  , caption_list :: ![String]
  , pics_list    :: ![String] }
  deriving Show

data Static = Static
  { proxies  :: ![Proxy]
  , captions :: ![FilePath]
  , pictures :: ![FilePath] }
  deriving Show

{-# INLINE _caps #-}
_caps = "./res/captions.conf"

{-# INLINE _pics #-}
_pics = "./res/pictures/"

{-# INLINE _proxy #-}
_proxy = "./res/proxy.conf"

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

toStatic :: Init -> Static 
toStatic Init{..} = Static proxy' caption_list pics
    where pairs = filter (not . null) . map (proxy_filter . proxy_pair) $ proxy_list
          Just proxy' = map (\(ip, port) -> Proxy (packChars ip) port) <$> sequence pairs
          pics = map (_pics <>) pics_list

buildStatic :: IO (Maybe Static)
buildStatic = do
    config <- init
    let static_config = toStatic <$> config
    case static_config of
      Left msg -> do
        putStrLn $ "[init] еггог, " <> msg
        pure Nothing
      Right st -> do
        putStrLn "[init] Ok, начальные данные успешно инициализированы."
        pure . Just $ st

get_proxy :: IO [String]
get_proxy = do
    state <- doesFileExist _proxy
    if not state
      then pure mempty
      else do
        content <- readFile _proxy
        pure . lines $ content

get_pictures :: IO [FilePath]
get_pictures = do
    state <- doesDirectoryExist _pics
    if not state
      then pure mempty
      else do
        content <- getDirectoryContents _pics
        pure $ filter (\x -> jpeg x || png x || jpg x) content
          where
            {-# INLINE jpeg #-}
            jpeg = isSuffixOf ".jpeg"

            {-# INLINE png #-}
            png = isSuffixOf ".png"

            {-# INLINE jpg #-}
            jpg = isSuffixOf ".jpg"

get_captions :: IO (Maybe [String])
get_captions = do
    state <- doesFileExist _caps
    if not state
      then pure Nothing
      else do
        content' <- BS.readFile _caps
        let content = unpackChars content'
        pure $ if null content
                 then Nothing
                 else Just . filter pred . splitOn "\n\n" $ content
                   where pred x = ((not . null $ x) && (x /= "\n"))

init :: IO (Either String Init)
init = do
    proxy <- get_proxy
    pics  <- get_pictures
    caps  <- get_captions

    let conf = Init <$> Just proxy
                    <*> caps
                    <*> Just pics
    pure $
      maybe (Left $ _caps <> " не найден либо пуст. Ошибка инициализации.")
            Right $ conf

