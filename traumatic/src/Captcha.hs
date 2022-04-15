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
perform :: Request -> IO (Maybe LBS.ByteString)
perform req = do
    manager <- newManager tlsManagerSettings
    response <- try (httpLbs req manager >>= pure . responseBody)
                     :: IO (Either HttpException LBS.ByteString)

    either (\_ -> pure Nothing)
           (\x -> pure . Just $ x) response

-- * main captcha types.
data CaptchaMeta = CaptchaMeta
  { captcha_type  :: AntiCaptchaType
  , captcha_key   :: !String
  , captcha_proxy :: Maybe Proxy }

instance Show CaptchaMeta where
    show CaptchaMeta{..} =
        case captcha_proxy of
          Nothing -> "[no_proxy]: "
          Just (Proxy{..}) ->
            "[" <> (BS.unpackChars proxyHost) <> ":" <> show proxyPort <> "]: "

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

getCaptchaId :: Maybe Proxy -> IO (Maybe MakabaCaptchaAnswer)
getCaptchaId proxy' = do
    request <- parseRequest $ base_captcha <> "id"
    response <- perform $ request { proxy = proxy' }
    return $ do
        result <- response
        decode result :: Maybe MakabaCaptchaAnswer

solveCaptcha :: CaptchaMeta -> LBS.ByteString -> IO String
solveCaptcha meta image = do
    let no_solver = error . (<> " solver isn't implemented yet.")
    case captcha_type meta of
        RuCaptcha ->
            solver_RuCaptcha meta image
        XCaptcha ->
            no_solver "XCaptcha" -- solver_XCaptcha captcha_key image
        AntiCaptcha ->
            no_solver "AntiCaptcha" -- solver_AntiCaptcha captcha_key image
        OCR ->
            no_solver "OCR" 

getCaptcha :: CaptchaMeta -> IO (Maybe Solved)
getCaptcha meta = do
    id_answer <- getCaptchaId (captcha_proxy meta)
    case id_answer of
      Nothing -> return Nothing
      Just MakabaCaptchaAnswer{..} -> do
        request <- parseRequest $ base_captcha <> "show?id=" <> captcha_id
        response <- perform (request { proxy = (captcha_proxy meta) })
        solvedMaybe <- maybe (pure Nothing) (\x -> solveCaptcha meta x >>= pure . Just) response

        return $ do
            solved <- solvedMaybe
            pure $ Solved captcha_id solved

-- | captcha solvers
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
solver_RuCaptcha_sendPost :: CaptchaMeta -> LBS.ByteString -> IO (Maybe RuCaptchaAnswer)
solver_RuCaptcha_sendPost meta image = do
    let body = [
            partBS "method" "post",
            partBS "key" . BS.packChars . captcha_key $ meta,
            partBS "json"   "1",
            partFileRequestBody "file" "file" $ RequestBodyLBS image
            ]
    request <- parseRequest "http://rucaptcha.com/in.php" >>= formDataBody body 
    response <- perform $ request { proxy = (captcha_proxy meta) }

    return $ do
        result <- response
        decode result :: Maybe RuCaptchaAnswer

-- check solver status.
solver_RuCaptcha_sendGet :: CaptchaMeta -> String -> IO (Maybe RuCaptchaAnswer)
solver_RuCaptcha_sendGet meta rucaptcha_id = do
    let link =  "http://rucaptcha.com/res.php?key=" <> (captcha_key meta) <> 
                "&action=get&json=1&id="            <> rucaptcha_id

    request <- parseRequest link 
    response <- perform $ request { proxy = (captcha_proxy meta) }

    return $ do
        result <- response
        decode result :: Maybe RuCaptchaAnswer

-- main check loop.
solver_RuCaptcha_handler :: CaptchaMeta -> RuCaptchaAnswer -> IO String
solver_RuCaptcha_handler meta ans@RuCaptchaAnswer{..} = do
    response <- solver_RuCaptcha_sendGet meta rucaptcha_request
    let solver_fail = pure "empty"
    case response of
      Nothing -> do
        putStrLn $ show meta <> "error, солвер выдал неожиданный ответ."
        solver_fail
      Just RuCaptchaAnswer{..} ->
        case rucaptcha_status of
          1 -> do
            putStrLn $ 
                show meta <> "CAPCHA_OK: капча решена успешно: " <> rucaptcha_request
            pure rucaptcha_request
          0 ->
            case rucaptcha_request of
              "CAPCHA_NOT_READY" ->
                solver_RuCaptcha_handler meta ans 
              "ERROR_WRONG_CAPTCHA_ID" -> do
                putStrLn $
                    show meta <> "SOLVER_FAILED: деньги кончились походу..."
                solver_fail
              "ERROR_WRONG_USER_KEY" -> do
                putStrLn $
                    show meta <> "KEY_FAILED: невалидный ключ."
                solver_fail
              other -> do
                putStrLn $
                    show meta <> other <> ": макак обхитрил индусов."
                solver_fail
          _ -> do
            putStrLn $
                show meta <> rucaptcha_request <> ": неожиданный код, остановочка."
            solver_fail

solver_RuCaptcha :: CaptchaMeta -> LBS.ByteString -> IO String
solver_RuCaptcha meta image = do
    post_answer <- solver_RuCaptcha_sendPost meta image
    case post_answer of
      Nothing -> pure "failed"
      Just ans -> do
        putStrLn (show meta <> "капча отправлена на решение...")
        solver_RuCaptcha_handler meta ans

