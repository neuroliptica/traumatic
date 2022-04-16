{-# OPTIONS_GHC -O2                                         #-}
{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards, DeriveAnyClass, DeriveGeneric #-}

module Engine
  ( PostMeta(..)
  , PostEnv(..)
  , Post(..)
  , MakabaResponse(..)
  , Thread(..)
  , Catalog(..)

  , performPost
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
import Control.Monad.Reader

import Prelude hiding (file)

infixr 9 <=>
(<=>) = partBS

{-# INLINE base_post #-}
base_post = "https://2ch.life/makaba/posting.fcgi?json=1"

{-# INLINE base_body #-}
base_body = [
        "task"            <=> "post",
        "usercode"        <=> none,
        "code"            <=> none,
        "captcha_type"    <=> "2chcaptcha",
        "oekaki_image"    <=> none,
        "oekaki_matadata" <=> none,
        "makaka_id"       <=> none,
        "makaka_answer"   <=> none
    ]

{-# INLINE none #-}
none :: BS.ByteString
none = ""

{-# INLINE ext #-}
ext :: String -> String
ext = ('.':) . reverse . fst . break (== '.') . reverse

data PostMeta = PostMeta
  { board  :: !String
  , thread :: !String }
  deriving Show

data Post = Post
  { meta        :: PostMeta
  , post_proxy  :: Maybe Proxy
  , text        :: !String 
  , files       :: ![FilePath]
  , post_sage   :: !Bool }
  deriving Show

data PostEnv = PostEnv Post Solved
    deriving Show

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
        where ip = maybe "no_proxy"
                         (\Proxy{..} -> BS.unpackChars proxyHost <> ":" <> show proxyPort)
                         current_proxy

{-# INLINE makeCont #-}
makeCont :: FilePath -> IO (LBS.ByteString, FilePath)
makeCont file = do
    name <- genFilename >>= pure . (<> ext file)
    cont <- LB.readFile file
    pure (cont, name)

{-# INLINE contToBody #-}
contToBody (cont, name) =
    partFileRequestBody "formimages[]" name $ RequestBodyLBS cont

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

getFiles (PostEnv Post {..} _) = files
getPostProxy (PostEnv Post {..} _) = post_proxy

getBoardBS (PostEnv Post {..} _) = BS.packChars . board $ meta
getThreadBS (PostEnv Post {..} _) = BS.packChars . thread $ meta
getSageBS (PostEnv Post {..} _) = BS.packChars $ if post_sage then "sage" else ""
getCommentBS (PostEnv Post {..} _) = BS.packChars text

getCaptchaIdBS (PostEnv _ (Solved captcha_id _)) = BS.packChars captcha_id
getCaptchaValueBS (PostEnv _ (Solved _ captcha_value)) = BS.packChars captcha_value

performPost :: Reader PostEnv (IO MakabaResponse)
performPost = do
    board <- asks getBoardBS
    thread <- asks getThreadBS
    sage <- asks getSageBS
    comment <- asks getCommentBS

    captcha_id <- asks getCaptchaIdBS
    captcha_value <- asks getCaptchaValueBS

    files <- asks getFiles
    postProxy <- asks getPostProxy

    let basePostBody = base_body ++ [ -- | several default body's fields.
            "board"            <=> board,
            "thread"           <=> thread,
            "email"            <=> sage,
            "comment"          <=> comment,
            "2chcaptcha_id"    <=> captcha_id,
            "2chcaptcha_value" <=> captcha_value
            ]

    return $ do
        baseRequest <- (\req -> req { proxy = postProxy }) <$> parseRequest base_post
        let maybeFiles = do
                maybeFilesM <- if null files
                                then Nothing
                                else Just files
                return $ do
                    conts <- mapM makeCont maybeFilesM
                    pure $ map contToBody conts

        parts <- maybe (pure Nothing)
                       (\x -> x >>= pure . Just) maybeFiles
        let finalBody = maybe basePostBody
                              (<> basePostBody) parts

        request <- formDataBody finalBody baseRequest
        response <- perform request

        let with = MakabaResponse postProxy
        -- Check 2ch server response.
        maybe (pure $ 404 `with` "запрос не удался, сервер вернул ошибку.")
              (postResponseHandler with)
              response

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
    let link = "https://2ch.hk/" <> board' <> "/threads.json"
    request <- parseRequest link
    response <- perform request
    return $ do
        catalog <- response
        decode catalog :: Maybe Catalog

