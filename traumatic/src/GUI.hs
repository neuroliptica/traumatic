{-# OPTIONS_GHC -O2                             #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-# LANGUAGE NoMonomorphismRestriction          #-}

module GUI
  ( guiMain
  ) where

import GI.Gtk hiding (main, init)
import GI.Gtk as Gtk (main, init)

import Control.Concurrent (forkIO)

import Control.Monad.IO.Class (MonadIO)
import GHC.Int                (Int32)
import Data.Text              (Text, pack, unpack)
import Data.Char              (isDigit)

import Init 
import Traumatic
import Static (Static(..))

-- * consts
{-# INLINE labelXalignDefault #-}
labelXalignDefault :: Float
labelXalignDefault = 0.02

{-# INLINE imageHeader #-}
imageHeader :: String
imageHeader = "./res/lain.png"

-- * helper utils.
main_window :: MonadIO m => Int32 -> Int32 -> m Window
main_window w h = do
    win <- windowNew WindowTypeToplevel
    -- * default main window settings:
    windowSetTitle       win "traumatic"
    windowSetDefaultSize win w h
    windowSetPosition    win WindowPositionCenter
    -- * callback:
    onWidgetDestroy      win mainQuit
    pure                 win

entry :: MonadIO m => Text -> m Entry
entry text = do
    entry_ <- entryNew
    entrySetPlaceholderText entry_ (Just text)
    pure entry_

set_editable_state master slave = do
    onToggleButtonToggled master $ do
        state <- toggleButtonGetActive master
        editableSetEditable slave (not state)

-- dunno why it's like this, but it works...
set_toggled_state master slave = do
    onToggleButtonToggled master $ do
        state <- toggleButtonGetActive slave
        if state
          then toggleButtonSetActive slave False
          else pure ()

        if state
          then pure ()
          else toggleButtonSetActive master True

set_toggled_three master s1 s2 s3 = do
    onToggleButtonToggled master $
        mapM_ (`toggleButtonSetActive` False)
            [ s1, s2, s3 ]

{-# INLINE get_field #-}
get_field :: MonadIO m => Entry -> m Text
get_field ent = getEntryText ent

{-# INLINE get_spin #-}
get_spin :: MonadIO m => SpinButton -> m Int
get_spin spin = do
    count <- spinButtonGetValue spin
    pure (round count :: Int)

{-# INLINE labelWithDefault #-}
labelWithDefault :: MonadIO m => Text -> m Label
labelWithDefault t = do
    label <- labelNew $ Just t
    labelSetXalign label labelXalignDefault
    pure label

-- * setting params:

--choose_mode :: MonadIO m => [(WipeMode, m CheckButton)] -> m WipeMode
choose_mode ((f,s):xs) = do
    state <- toggleButtonGetActive s
    if state || xs == []
      then pure f
      else choose_mode xs

guiMain :: Static -> IO ()
guiMain static = do
    -- static <- buildStatic

    Gtk.init Nothing
    -- * main window:
    main_win <- main_window 400 550

    -- * main vertical paned:
    vpaned <- panedNew OrientationVertical
    containerAdd main_win vpaned

    -- * first main frame and it's framebox:
    first_frame <- frameNew Nothing
    first_framebox <- boxNew OrientationVertical 1

    -- * pack frame to paned, then framebox to frame:
    panedPack1 vpaned first_frame False False
    containerAdd first_frame first_framebox

    image <- imageNewFromFile imageHeader
    containerAdd first_framebox image

    -- * mode settings:

    mode_frame <- frameNew $ Just " Режим:"
    mode_framebox <- boxNew OrientationHorizontal 1
    containerAdd mode_frame mode_framebox
    containerAdd first_framebox mode_frame
    
    mode_SingleThread <- checkButtonNewWithLabel "Один тред"
    mode_Shrapnel <- checkButtonNewWithLabel "Шрапнель (вся доска)"
    
    mapM_ (containerAdd mode_framebox)
        [ mode_SingleThread, mode_Shrapnel ]

    toggleButtonSetActive mode_SingleThread True

    set_toggled_state mode_SingleThread mode_Shrapnel
    set_toggled_state mode_Shrapnel mode_SingleThread

    -----------
    -- * proxy settings:

    proxy_frame <- frameNew $ Just " Прокси:"
    proxy_framebox <- boxNew OrientationHorizontal 1
    containerAdd proxy_frame proxy_framebox
    containerAdd first_framebox proxy_frame 

    proxy_NoProxy <- checkButtonNewWithLabel "Без проксей"
    proxy_WithProxy <- checkButtonNewWithLabel "Взять из ./res/proxy.conf"

    mapM_ (containerAdd proxy_framebox)
        [ proxy_NoProxy, proxy_WithProxy ]

    toggleButtonSetActive proxy_WithProxy True

    set_toggled_state proxy_WithProxy proxy_NoProxy
    set_toggled_state proxy_NoProxy proxy_WithProxy

    ------------
    -- * post meta
    
    meta_frame <- frameNew $ Just " Мета:"
    meta_framebox <- boxNew OrientationVertical 1
    containerAdd meta_frame meta_framebox
    containerAdd first_framebox meta_frame

    board_entry <- entry "Доска. По дефолту b."
    thread_entry <- entry "Id треда. (Если режим SingleThread)"

    set_editable_state mode_Shrapnel thread_entry

    mapM_ (containerAdd meta_framebox)
        [ board_entry, thread_entry ]

    ------------
    -- * post settings

    settings_frame <- frameNew $ Just " Пост:"
    settings_framebox <- boxNew OrientationHorizontal 1
    containerAdd settings_frame settings_framebox
    containerAdd first_framebox settings_frame

    settings_sage <- checkButtonNewWithLabel "Sage"
    settings_pics <- checkButtonNewWithLabel "Картинка (./res/pictures/)"

    mapM_ (containerAdd settings_framebox)
        [ settings_pics, settings_sage ]
    ------------

    ------------
    -- * timing settings 

    main_delay_frame <- frameNew Nothing
    main_delay_framebox <- boxNew OrientationHorizontal 5
    containerAdd main_delay_frame main_delay_framebox
    containerAdd first_framebox main_delay_frame

    -- threads

    thread_frame <- frameNew $ Just " Потоки (max 500): "
    thread_framebox <- boxNew OrientationVertical 1
    containerAdd thread_frame thread_framebox
    containerAdd main_delay_framebox thread_frame

    thread_spin <- spinButtonNewWithRange 1 500 1
    containerAdd thread_framebox thread_spin

    -- count

    count_frame <- frameNew $ Just " Кол-во проходов: "
    count_framebox <- boxNew OrientationVertical 1
    containerAdd count_frame count_framebox

    count_spin <- spinButtonNewWithRange 1 1000 1
    containerAdd count_framebox count_spin
    containerAdd main_delay_framebox count_frame 

    -- delay

    delay_frame <- frameNew $ Just " Перерыв (сек): "
    delay_framebox <- boxNew OrientationVertical 1
    containerAdd delay_frame delay_framebox
    containerAdd main_delay_framebox delay_frame

    delay_spin <- spinButtonNewWithRange 0 1000 1
    containerAdd delay_framebox delay_spin 

    ------------
    -- * anti captcha settings

    captcha_frame <- frameNew $ Just " Антикапча:"
    captcha_framebox <- boxNew OrientationHorizontal 1
    containerAdd captcha_frame captcha_framebox
    containerAdd first_framebox captcha_frame

    captcha_rucaptcha <- checkButtonNewWithLabel "RuCaptcha"
    captcha_xcaptcha <- checkButtonNewWithLabel "XCaptcha"
    captcha_anticaptcha <- checkButtonNewWithLabel "AntiCaptcha"
    captcha_ocr <- checkButtonNewWithLabel "OCR"

    mapM_ (containerAdd captcha_framebox)
        [ captcha_rucaptcha, captcha_xcaptcha, captcha_anticaptcha, captcha_ocr ]

    toggleButtonSetActive captcha_rucaptcha True

    set_toggled_three captcha_rucaptcha captcha_anticaptcha captcha_ocr captcha_xcaptcha
    set_toggled_three captcha_anticaptcha captcha_rucaptcha captcha_xcaptcha captcha_ocr
    set_toggled_three captcha_xcaptcha captcha_ocr captcha_rucaptcha captcha_anticaptcha
    set_toggled_three captcha_ocr captcha_anticaptcha captcha_rucaptcha captcha_xcaptcha
    
    ------------
    -- * anti captcha key

    key_frame <- frameNew $ Just " Ключ антикапчи:"
    key_framebox <- boxNew OrientationVertical 1
    containerAdd key_frame key_framebox
    containerAdd first_framebox key_frame

    key_entry <- entry "API ключик антикапчи, если не OCR."
    containerAdd key_framebox key_entry
    ------------


    -- * buttons frame
    buttons_frame <- frameNew Nothing
    buttons_framebox <- boxNew OrientationHorizontal 5
    boxSetHomogeneous buttons_framebox True

    containerAdd buttons_frame buttons_framebox
    containerAdd first_framebox buttons_frame

    -- * start button

    main_button <- buttonNewWithLabel "Пуск"
    containerAdd buttons_framebox main_button

    -- * stop button
    --stop_button <- buttonNewWithLabel "Стоп"
    --containerAdd buttons_framebox stop_button
    -----------------

    onButtonClicked main_button $ do
        params <-
            InitParams
                <$> choose_mode [(SingleThread, mode_SingleThread), (Shrapnel, mode_Shrapnel)]               
                <*> pure RuCaptcha -- other not implemented yet.
                <*> choose_mode [(NoProxy, proxy_NoProxy), (WithProxy, proxy_WithProxy)]
                <*> pure "b"
                <*> pure Nothing
                <*> toggleButtonGetActive settings_sage
                <*> toggleButtonGetActive settings_pics
                <*> get_spin thread_spin
                <*> get_spin count_spin
                <*> get_spin delay_spin
                <*> (get_field key_entry >>= pure . unpack)

        params' <-
            case wipe_mode params of
                SingleThread ->
                  (\x -> params { init_thread = Just x })
                      <$> (get_field thread_entry >>= pure . unpack)
                _ ->
                  pure params
        custom_board <-
           get_field board_entry >>= pure . unpack
         
        let
          final_params =
            if null custom_board
              then params'
              else params' { init_board = custom_board }

        let thread' = init_thread final_params
        let
          f = 
            (\_ ->
             case thread' of
              Just t ->
                if null t || any (not . isDigit) t
                  then do
                    putStrLn "Ошибка инициализации, тред указан не верно."
                    pure ()
                  else do
                    traumatic static final_params
              Nothing ->
                traumatic static final_params )

        -- so GUI won't stack
        forkIO $ f () 
        pure ()

    widgetShowAll main_win
    Gtk.main

