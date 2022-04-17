{-# OPTIONS_GHC -O2          #-}
{-# LANGUAGE RecordWildCards #-}

module Static
  ( Static(..)
  , buildStatic 
  ) where

import Network.HTTP.Client (Proxy(..))

import qualified Data.ByteString as BS (readFile)
import        Data.ByteString.Internal (packChars, unpackChars)

import Data.List.Split (splitOn)
import Data.List       (isSuffixOf)
import Data.Char       (isDigit)

import System.Directory (doesFileExist, doesDirectoryExist, getDirectoryContents)

import Prelude hiding (init, pairs)

import Control.Applicative
import Control.Monad (mapM)

import Parser

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

_caps  = "./res/captions.conf"
_pics  = "./res/pictures/"
_proxy = "./res/proxy.conf"

parseProxy :: String -> Maybe Proxy
parseProxy str = do
    (adr@(IP _ _ _ _ p), _) <- runParser ip str
    let ipAdr = address adr
    if null p
      then Nothing
      else Just $ Proxy (packChars ipAdr) (read p :: Int)

parseAuthProxy :: String -> Maybe Proxy
parseAuthProxy str = do
    (adr@(Auth login pass adr_ip@(IP _ _ _ _ p)), _) <- runParser auth_ip str
    let ipAdr = address adr_ip
    if null p
      then Nothing
      else Just $ Proxy (packChars $ login <> ":" <> pass <> "@" <> ipAdr)
                        (read p :: Int)

parseP str = parseProxy str <|> parseAuthProxy str

toStatic :: Init -> Static
toStatic Init{..} =
    let
        Just proxy = sequence . filter (not . null) $ map parseP proxy_list
        pics       = map (_pics <>) pics_list
    in
        Static proxy caption_list pics

buildStatic :: IO (Maybe Static)
buildStatic = do
    config <- init
    let static_config = toStatic <$> config
    let init_ok = "[init] Ok, начальные данные успешно инициализированы."
    maybe (pure Nothing)
          (\st -> putStrLn init_ok >> (pure $ Just st)) 
          static_config

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

init :: IO (Maybe Init)
init = do
    proxy <- get_proxy
    pics  <- get_pictures
    caps  <- get_captions

    let conf = Init <$> Just proxy
                    <*> caps
                    <*> Just pics
    let emptyConf = "[init] Error, " <> _caps <>
                    " не найден либо пуст. Ошибка инициализации."

    maybe (putStrLn emptyConf >> pure Nothing) (pure . Just) conf

