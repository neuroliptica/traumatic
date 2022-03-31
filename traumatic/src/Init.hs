{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards           #-}

module Init
  ( InitParams(..)
  , AntiCaptchaType(..)
  , WipeMode(..)
  , ProxyMode(..)
   -- * Init from args parse function.
  , parseInit
  ) where

import Text.Read (readMaybe)

data AntiCaptchaType
  = RuCaptcha
  | XCaptcha
  | AntiCaptcha
  | OCR
  deriving (Eq, Show, Read)

data WipeMode
  = SingleThread
  | Shrapnel
  deriving (Eq, Read)

instance Show WipeMode where
    show SingleThread = "Один тред."
    show Shrapnel     = "Шрапнель (вся доска)."

data ProxyMode
  = NoProxy
  | WithProxy
  deriving (Eq, Read)

instance Show ProxyMode where
    show NoProxy   = "Без проксей."
    show WithProxy = "Взять из дефолтного конфига."

data InitParams = InitParams
  { wipe_mode         :: WipeMode
  , anti_captcha_type :: AntiCaptchaType
  , proxy_mode        :: ProxyMode
  , init_board        :: !String
  , init_thread       :: Maybe String
  , append_sage       :: !Bool
  , append_pic        :: !Bool
  , threads_count     :: !Int
  , anti_captcha_key  :: !String }

instance Show InitParams where
    show InitParams{..} = 
        "[InitParams]:" <>
        "\n-----------------------------------" <>
        "\n|\tРежим = " <> show wipe_mode <>
        "\n|\tДоска = " <> init_board <>
        "\n|\tТред = " <> maybe ("Все и понемношку.") id init_thread <>
        "\n|\tСажа = " <> (if append_sage then "Да." else "Нет.") <>
        "\n|\tС картинкой = " <> (if append_pic then "Да." else "Нет.") <>
        "\n|\tАнти капча = " <> show anti_captcha_type <>
        "\n|\tПрокси = " <> show proxy_mode <>
        "\n|\tКол-во потоков на одну проксю = " <> show threads_count <>
        "\n-----------------------------------"

parseArg :: String -> Maybe (String, String)
parseArg arg =
    let (key, value) = break (== '=') arg in
    if null value || (null . tail $ value)
      then Nothing
      else Just (format key, tail value)
        where
        {-# INLINE format #-}
        format :: String -> String
        format [] = []
        format (x:xs) | x /= '-'  = x:xs
                      | otherwise = format xs

{-# INLINE defaultInit #-}
defaultInit :: InitParams
defaultInit = InitParams
              { anti_captcha_type = RuCaptcha
              , proxy_mode        = WithProxy
              , append_sage       = False
              , append_pic        = False
              , threads_count     = 1
              , anti_captcha_key  = "no key"
              -- вайпать весь b по дефолту.
              , wipe_mode   = Shrapnel
              , init_board  = "b"
              , init_thread = Nothing }

{-# INLINE comp #-}
comp :: Eq a => (InitParams -> a) -> InitParams -> InitParams -> a
comp cmp init1 init2 =
    if cmp init1 == cmp init2 || cmp init2 == cmp defaultInit
      then cmp init1
      else cmp init2

instance Semigroup InitParams where
    (<>) init1 init2 =
        let choose_field = (\x -> comp x init1 init2)
        in InitParams
            { anti_captcha_type = choose_field anti_captcha_type
            , wipe_mode = choose_field wipe_mode
            , proxy_mode = choose_field proxy_mode 
            , init_board = choose_field init_board
            , init_thread = choose_field init_thread 
            , append_sage = choose_field append_sage
            , append_pic = choose_field append_pic
            , threads_count = choose_field threads_count
            , anti_captcha_key = choose_field anti_captcha_key }

instance Monoid InitParams where
    mempty = defaultInit

appendInitKey :: (String, String) -> InitParams
appendInitKey (key, value) =
    case key of
      "captcha" ->
        maybe mempty (\x -> mempty { anti_captcha_type = x } )
                     $ (readMaybe value :: Maybe AntiCaptchaType)
      "mode" ->
        maybe mempty (\x -> mempty { wipe_mode = x } )
                     $ (readMaybe value :: Maybe WipeMode)
      "proxy" ->
        maybe mempty (\x -> mempty { proxy_mode = x } )
                     $ (readMaybe value :: Maybe ProxyMode)
      "board" ->
        mempty { init_board = value }
      "thread" ->
        maybe mempty (\x -> mempty { init_thread = Just $ show x } )
                     $ (readMaybe value :: Maybe Int)     
      "sage" ->
        maybe mempty (\x -> mempty { append_sage = x } )
                     $ (readMaybe value :: Maybe Bool)
      "pics" ->
        maybe mempty (\x -> mempty { append_pic = x } )
                     $ (readMaybe value :: Maybe Bool)
      "threads" ->
        maybe mempty (\x -> mempty { threads_count = x } )
                     $ (readMaybe value :: Maybe Int)
      "key" ->
        mempty { anti_captcha_key = value }
      _     -> mempty

parseInit :: [String] -> InitParams
parseInit = maybe mempty id . (mconcat <$>) . (map appendInitKey <$>) . sequence . filter (not . null) . map parseArg

