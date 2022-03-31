{-# OPTIONS_GHC -O2                             #-}
{-# LANGUAGE DeriveAnyClass, DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-# LANGUAGE NoMonomorphismRestriction          #-}

module Captcha
  ( CaptchaMeta(..)
  , Solved(..)
  , getCaptcha
   -- * helper utils.
  , perform
  , base_captcha
  , get_random
  , genFilename
  ) where

import Data.Aeson   (FromJSON(..), decode, withObject, (.:))
import GHC.Generics (Generic)

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Client.MultipartFormData

import qualified Data.ByteString.Lazy          as LBS
import qualified Data.ByteString.Internal      as BS
import qualified Data.ByteString               as B

import System.Random     (randomRIO)

import Control.Monad     (replicateM)
import Control.Exception (try)

import Init (AntiCaptchaType(..)) 

{-# INLINE get_random #-}
get_random :: [a] -> IO a
get_random xs = randomRIO (0, length xs - 1) >>= pure . (xs !!)

{-# INLINE random_name #-}
random_name :: Int -> IO String
random_name n = replicateM n (get_random template)
    where template = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"

{-# INLINE genFilename #-}
genFilename :: IO String
genFilename = randomRIO (20, 60) >>= random_name

-- * consts.
{-# INLINE base_captcha #-}
base_captcha = "https://2ch.life/api/captcha/2chcaptcha/"

-- * helper utils.
{-# INLINE perform #-}
perform :: Request -> IO (Either HttpException LBS.ByteString)
perform req = do
    manager <- newManager tlsManagerSettings
    try (httpLbs req manager >>= pure . responseBody)

-- * main captcha types.
data CaptchaMeta = CaptchaMeta
  { captcha_type :: AntiCaptchaType
  , captcha_key  :: !String }
  deriving Show

-- 2ch server get captcha answer.
data MakabaCaptchaAnswer = MakabaCaptchaAnswer
  { captcha_id :: !String 
  , _input     :: !String
  , _result    :: !Int
  , _type      :: !String }
  deriving (Show, Generic)

data Solved = Solved !String !String
  deriving Show

instance FromJSON MakabaCaptchaAnswer where
    parseJSON = withObject "MakabaCaptchaAnswer" $ \v -> MakabaCaptchaAnswer
        <$> v .: "id"
        <*> v .: "input"
        <*> v .: "result"
        <*> v .: "type"

getCaptchaId :: IO (Maybe MakabaCaptchaAnswer)
getCaptchaId = do
    request <- parseRequest $ base_captcha <> "id"
    response <- perform request
    let
      result = 
        (\x -> decode x :: Maybe MakabaCaptchaAnswer)
            <$> response
    either (\_ -> pure Nothing)
           (pure . id) $ result

solveCaptcha :: CaptchaMeta -> LBS.ByteString -> IO String
solveCaptcha CaptchaMeta{..} image = do
    let no_solver = error . (<> " solver isn't implemented yet.")
    case captcha_type of
        RuCaptcha ->
            solver_RuCaptcha captcha_key image
        XCaptcha ->
            no_solver "XCaptcha" -- solver_XCaptcha captcha_key image
        AntiCaptcha ->
            no_solver "AntiCaptcha" -- solver_AntiCaptcha captcha_key image
        OCR ->
            no_solver "OCR" 

getCaptcha :: CaptchaMeta -> IO (Maybe Solved)
getCaptcha meta = do
    id_answer <- getCaptchaId
    case id_answer of
      Nothing -> pure Nothing    
      Just MakabaCaptchaAnswer{..} -> do
        request <- parseRequest $ base_captcha <> "show?id=" <> captcha_id
        result <- perform request
                    >>= pure . (solveCaptcha meta <$>)
        either (\_ -> pure Nothing)
               (\res -> res >>= pure . Just . Solved captcha_id)
                   $ result

-------------------- captcha solvers
--- RuCaptcha:

data RuCaptchaAnswer = RuCaptchaAnswer
  { rucaptcha_status  :: !Int
  , rucaptcha_request :: !String }
  deriving (Show, Generic)

instance FromJSON RuCaptchaAnswer where
    parseJSON = withObject "RuCaptchaAnswer" $ \v -> RuCaptchaAnswer
        <$> v .: "status"
        <*> v .: "request"

-- send captcha to solver
solver_RuCaptcha_sendPost :: String -> LBS.ByteString -> IO (Maybe RuCaptchaAnswer)
solver_RuCaptcha_sendPost api_key image = do
    let body = [
            partBS "method" "post",
            partBS "key" $ BS.packChars api_key,
            partBS "json" "1",
            partFileRequestBody "file" "file" $ RequestBodyLBS image
            ]
    request <- parseRequest "http://rucaptcha.com/in.php"
                    >>= formDataBody body 
    response <- perform request
    let
      result = 
        (\x -> decode x :: Maybe RuCaptchaAnswer)
            <$> response
    either (\_ -> pure Nothing)
           (pure . id) $ result

-- check solver status.
solver_RuCaptcha_sendGet :: String -> String -> IO (Maybe RuCaptchaAnswer)
solver_RuCaptcha_sendGet api_key rucaptcha_id = do
    let
      link = 
        "http://rucaptcha.com/res.php?key=" <>
         api_key <> "&action=get&json=1&id="<> rucaptcha_id
    response <- parseRequest link >>= perform
    let
      result =
        (\x -> decode x :: Maybe RuCaptchaAnswer)
            <$> response
    either (\_ -> pure Nothing)
           (pure . id) $ result

-- main check loop.
solver_RuCaptcha_handler :: String -> RuCaptchaAnswer -> IO String
solver_RuCaptcha_handler api_key ans@RuCaptchaAnswer{..} = do
    response <- solver_RuCaptcha_sendGet api_key rucaptcha_request
    let solver_fail = pure "empty"
    case response of
      Nothing -> do
        putStrLn "[RuCaptcha]: еггог, солвер выдал неожиданный ответ!!"
        solver_fail
      Just RuCaptchaAnswer{..} ->
        case rucaptcha_status of
          1 -> do
            putStrLn $ 
                "[RuCaptcha] CAPCHA_OK: капча решена успешно: " <> rucaptcha_request
            pure rucaptcha_request
          0 ->
            case rucaptcha_request of
              "CAPCHA_NOT_READY" ->
                solver_RuCaptcha_handler api_key ans 
              "ERROR_WRONG_CAPTCHA_ID" -> do
                putStrLn $
                    "[RuCaptcha] SOLVER_FAILED: деньги кончились походу..."
                solver_fail
              "ERROR_WRONG_USER_KEY" -> do
                putStrLn $
                    "[RuCaptcha] KEY_FAILED: невалидный ключ."
                solver_fail
              other -> do
                putStrLn $
                    "[RuCaptcha] " <> other <> ": макак обхитрил индусов."
                solver_fail
          _ -> do
            putStrLn $
                "[RuCaptcha] " <> rucaptcha_request <> ": неожиданный код, остановочка."
            solver_fail

solver_RuCaptcha :: String -> LBS.ByteString -> IO String
solver_RuCaptcha api_key image = do
    post_answer <- solver_RuCaptcha_sendPost api_key image
    maybe (pure "failed")
          (\ans -> putStrLn "[RuCaptcha]: капча отправлена на решение..."
                        >> solver_RuCaptcha_handler api_key ans)
                            $ post_answer

------------------------------------

{- data Captcha = Captcha
  { api_answer    :: MakabaCaptchaAnswer
  , captcha_value :: !String }
  deriving Show

instance FromJSON MakabaCaptchaAnswer where
    parseJSON = withObject "MakabaCaptchaAnswer" $ \v -> CaptchaAnswer
        <$> v .: "id"
        <*> v .: "input"
        <*> v .: "result"
        <*> v .: "type"

data RuCaptchaAnswer = RuCaptchaAnswer
  { captcha_status  :: !Int
  , captcha_request :: !String }
  deriving (Show, Generic)

instance FromJSON RuCaptchaAnswer where
    parseJSON = withObject "RuCaptchaAnswer" $ \v -> RuCaptchaAnswer
        <$> v .: "status"
        <*> v .: "request"

-- * main captcha methods.
getCaptchaId :: IO (Maybe MakabaCaptchaAnswer)
getCaptchaId = do
    request <- parseRequest $ base_captcha <> "id"
    response <- perform request
    pure (decode response :: Maybe MakabaCaptchaAnswer)

sendPost :: String -> LBS.ByteString -> IO (Maybe RuCaptchaAnswer) 
sendPost api_key image = do
    let body = [
            partBS "method" "post",
            partBS "key" $ BS.packChars api_key,
            partBS "json" "1",
            partFileRequestBody "file" "file" $ RequestBodyLBS image
            ]
    request <- parseRequest "http://rucaptcha.com/in.php"
                    >>= formDataBody body 
    response <- perform request
    pure (decode response :: Maybe RuCaptchaAnswer)

sendGet :: String -> String -> IO (Maybe RuCaptchaAnswer) 
sendGet api_key _id = do
    let
      link = 
        "http://rucaptcha.com/res.php?key=" <>
         api_key <> "&action=get&json=1&id="<> _id
    response <- parseRequest link >>= perform
    pure (decode response :: Maybe RuCaptchaAnswer)

handle :: String -> RuCaptchaAnswer -> IO String
handle api_key res@RuCaptchaAnswer{..} = do
    response <- sendGet api_key captcha_request
    case response of
      Nothing -> do
        putStrLn "Еггог, солвер выдал неожиданный ответ!!"
        pure "empty"
      Just RuCaptchaAnswer{..} ->
        case captcha_status of
          1 -> do
            putStrLn "CAPCHA_OK: капча решена успешно."
            pure captcha_request
          0 ->
            case captcha_request of
              "CAPCHA_NOT_READY" -> handle api_key res
              other -> do
                putStrLn $ other <> ": макак обхитрил индусов."
                pure "empty"
          c -> do
            putStrLn $ show c <> ": неожиданный код, остановочка."
            pure "empty"

solveCaptcha :: MakabaCaptchaAnswer -> IO String
solveCaptcha MakabaCaptchaAnswer{..} = do
    request <- parseRequest $ base_captcha <> "show?id=" <> captcha_id
    image <- perform request
    let key = "fbb7a2fe74030d7bd4627722126eeb12"

    try <- sendPost key image 
    maybe (pure "captcha failed")
          (\ans -> putStrLn "[captcha] init, капча отправлена индусам..."
                            >> handle key ans) 
                                $ try

getCaptcha :: IO (Maybe Captcha)
getCaptcha = do
    answer <- getCaptchaId
    solved <- pure (solveCaptcha <$> answer)
    maybe (pure Nothing)
          (\value -> value >>= (pure . (Captcha <$> answer <*>) . Just))
                $ solved
 -}


