{-# OPTIONS_GHC -O2 #-}

module Config
  (
    Config(..)
  , initConfig
   -- * should be exported coz we're using it.
  , _pics
  ) where

import System.Directory
import Data.List       (isSuffixOf)
import Data.List.Split (splitOn)

import qualified Data.ByteString          as BS
import qualified Data.ByteString.Internal as BSI

-- Encoding is UTF-8 by default.
{-# INLINE _caps #-}
_caps :: FilePath
_caps = "./res/captions.conf"

{-# INLINE _pics #-}
_pics :: FilePath
_pics = "./res/pictures/"

{-# INLINE _proxy #-}
_proxy :: FilePath
_proxy = "./res/proxy.conf"

-- Main parse result type. Email should be removed btw.
data Config = Config
  { proxy_list    :: ![String]
  , caption_list  :: ![String]
  , pics_list     :: ![String]
  , email         :: !String }
  deriving Show

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
        let content = BSI.unpackChars content'
        pure $ if null content
                 then Nothing
                 else Just . filter pred . splitOn "\n\n" $ content
                   where pred x = ((not . null $ x) && (x /= "\n"))

initConfig :: IO (Either String Config)
initConfig = do
    proxy <- get_proxy
    pics  <- get_pictures
    caps  <- get_captions

    let conf = Config <$> Just proxy
                      <*> caps
                      <*> Just pics
                      <*> Just mempty 
    pure $
      maybe (Left $ _caps <> " не найден либо пуст. Ошибка инициализации.")
            (\conf' -> Right conf')
                $ conf

