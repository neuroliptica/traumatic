module Main
  where

import GUI
import Traumatic

import Init

import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    if (any (== "--help") $ args) || (any (== "-h") $ args)
      then
        putStrLn help
      else do
        putStrLn art
        static <- buildStatic
        args <- getArgs
        let
          initFunction =
            if any (== "--no-gui") $ args
              then (`traumatic` (parseInit args))
              else guiMain
        maybe (pure ()) (\s -> initFunction s)
            $ static

art = "\n" <>
      " __ __| _ \\    \\    |  |  \\  |    \\ __ __| _ _|   __| \n" <>
      "    |     /   _ \\   |  | |\\/ |   _ \\   |     |   (    \n" <>
      "   _|  _|_\\ _/  _\\ \\__/ _|  _| _/  _\\ _|   ___| \\___| \n"

help = "Использование ./traumatic-exe [флаги настройки]\n" <>
       "Флаги:\n" <>
       "\t --no-gui - не запускать графический интерфейс.\n" <>
       "\t --captcha=<type> - задать сервис антикапчи. RuCaptcha; XCaptcha; AntiCaptcha; OCR. \n\t\tDefault: --captcha=RuCaptcha.\n" <>
       "\t --mode=<type> - задать режим вайпа. SingleThread; Shrapnel. \n\t\tDefault: --mode=Shrapnel.\n" <>
       "\t --proxy=<type> - с проксями или без. NoProxy; WithProxy. \n\t\tDefault --proxy=WithProxy.\n" <>
       "\t --board=<board> - доска. \n\t\tDefault: --board=b.\n" <>
       "\t --thread=<num> - id треда. \n\t\tDefault: никакой, т.к. режим шрапнели.\n" <>
       "\t --sage=<bool> - клеить ли сажу. False - нет; True - да. \n\t\tDefault: --sage=False.\n" <>
       "\t --pics=<bool> - клеить ли картинку. False - нет; True - да. \n\t\tDefault: --pics=False.\n" <>
       "\t --threads=<num> - кол-во потоков на одну проксю. \n\t\tDefault: --threads=1.\n" <>
       "\t --key=<key> - ключ для сервиса антикапчи, если не OCR. \n\t\tDefault: --key= , то бишь никакой.\n" <>
       "\t -h, --help - показать это сообщение.\n" <>
       "Вроде всё. По всем вопросам пишите в редакцию в телеграме: @seharehare"

