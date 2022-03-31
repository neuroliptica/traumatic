{-# OPTIONS_GHC -O2                                         #-}
{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards, DeriveAnyClass, DeriveGeneric #-}

module Engine
  ( PostMeta(..)
  , Post(..)
  , MakabaResponse(..)
  -- * main perform function.
  , performPost
  , Thread(..)
  , Catalog(..)
  , getThreads
  ) where

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Client.MultipartFormData

import qualified Data.ByteString.Lazy.Internal as LBS
import qualified Data.ByteString.Internal      as BS
import qualified Data.ByteString.Lazy          as LB (readFile)

import Data.Text (Text, pack)

import Data.Aeson   (FromJSON(..), withObject, decode, (.:))
import GHC.Generics (Generic)

import Captcha

import Prelude hiding (file)

-- * consts.
{-# INLINE base_post #-}
base_post = "https://2ch.life/makaba/posting.fcgi?json=1"

{-# INLINE none #-}
none :: BS.ByteString
none = ""

{-# INLINE ext #-}
ext :: String -> String
ext = ('.':) . reverse . fst . break (== '.') . reverse

-- * main types.
data PostMeta = PostMeta
  { board  :: !String
  , thread :: !String }
  deriving Show

data Post = Post
  { meta       :: PostMeta
  , post_proxy :: Maybe Proxy
  , text       :: !String 
  , file       :: Maybe FilePath
  , post_sage  :: !Bool }
  deriving Show

-- * 2ch api json response types.
data PostOk = PostOk
  { empty_error :: Maybe Int
  , status      :: !String
  , post_num    :: !Int }
  deriving (Show, Generic)

instance FromJSON PostOk where
    parseJSON = withObject "PostOk" $ \v -> PostOk
        <$> v .: "Error"
        <*> v .: "Status"
        <*> v .: "Num"

data PostFailed = PostFailed
  { error_code :: !Int
  , reason     :: !String }
  deriving (Show, Generic)

instance FromJSON PostFailed where
    parseJSON = withObject "PostFailed" $ \v -> PostFailed
        <$> v .: "Error"
        <*> v .: "Reason"

data MakabaResponse = MakabaResponse
  { current_proxy :: Maybe Proxy
  , err_code      :: !Int
  , message       :: !String }

instance Show MakabaResponse where
    show MakabaResponse{..} =
        "[" <> ip <> "] [" <> show err_code <> "]: " <> message
        where ip = maybe "no_proxy" (\Proxy{..} ->
                   (BS.unpackChars proxyHost) <> ":" <> show proxyPort)
                       $ current_proxy

-- assign field value in multipart body.
infixr 9 <=>
(<=>) = partBS

postResponseHandler
                 :: (Int -> String -> MakabaResponse)
                 -> LBS.ByteString 
                 -> IO MakabaResponse
postResponseHandler with response = do
    pure $
      case decode response :: Maybe PostOk of
        Just PostOk{..} ->
          200 `with` ("пост отправлен. Id = " <> show post_num)
        Nothing ->
          case decode response :: Maybe PostFailed of
            Just PostFailed{..} ->
              error_code `with` reason
            Nothing ->
              404 `with` "неизвестная ошибка, пост не отправлен."

-- * main requests.
performPost :: Post -> Solved -> IO MakabaResponse 
performPost Post{..} (Solved captcha_id captcha_value) = do
    base_request' <- parseRequest base_post
    let base_request = base_request'
                       { proxy = post_proxy }
    let base_body = [
            "task"            <=> "post",
            "board"           <=> (BS.packChars . board $ meta),
            "thread"          <=> (BS.packChars . thread $ meta),
            "usercode"        <=> none,
            "code"            <=> none,
            "captcha_type"    <=> "2chcaptcha",
            "oekaki_image"    <=> none,
            "oekaki_matadata" <=> none,
            "email"           <=> (if post_sage then (BS.packChars "sage") else none),
            "comment"         <=> (BS.packChars text),
            "makaka_id"       <=> none,
            "makaka_answer"   <=> none,

            "2chcaptcha_id"    <=> (BS.packChars captcha_id),
            "2chcaptcha_value" <=> (BS.packChars captcha_value),

            "comment"         <=> (BS.packChars text)
            ]

    maybeFile <- do
        case file of
            Nothing ->
              pure Nothing
            Just file' -> do
              name' <- genFilename >>= pure . (<> ext file')
              cont <- LB.readFile file'
              pure $ Just (cont, name')

    -- If we have file then attach, otherwise will ignore.
    let
      image_to_post =
        (\(cont, name') -> partFileRequestBody "formimages[]" name' $ RequestBodyLBS cont)
            <$> maybeFile
    let
      final_body = 
        maybe base_body (\part -> part : base_body) image_to_post

    request <- formDataBody final_body base_request
    response <- perform request
    
    let
      with =
        MakabaResponse post_proxy
    -- Check 2ch server response.
    either (\_ -> pure $ 404 `with` "запрос не удался, сервер вернул ошипку.")
           (postResponseHandler with)
               $ response

data Thread = Thread
  { _comment     :: !String
  , _lasthit     :: !Int
  , _num         :: !String
  , _posts_count :: !Int
  , _score       :: !Double
  , _subject     :: !String
  , _timestamp   :: !Int
  , _views       :: !Int }
  deriving (Show, Generic)

data Catalog = Catalog
  { _board   :: !String
  , _threads :: ![Thread] }
  deriving (Show, Generic)

instance FromJSON Thread where
    parseJSON = withObject "Thread" $ \v -> Thread
        <$> v .: "comment"
        <*> v .: "lasthit"
        <*> v .: "num"
        <*> v .: "posts_count"
        <*> v .: "score"
        <*> v .: "subject"
        <*> v .: "timestamp"
        <*> v .: "views"

instance FromJSON Catalog where
    parseJSON = withObject "Catalog" $ \v -> Catalog
        <$> v .: "board"
        <*> v .: "threads"

getThreads :: String -> IO (Maybe Catalog)
getThreads board' = do
    let
      link =
        "https://2ch.hk/" <> board' <> "/threads.json"
    request <- parseRequest link
    response <- perform request
    pure $
        either (\_ -> Nothing)
               (\x -> decode x :: Maybe Catalog)
                  $ response

