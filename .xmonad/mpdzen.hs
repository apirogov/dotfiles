#!/usr/bin/env stack
-- stack script --resolver lts-9.0 --package shelly,text,directory,time
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
import Data.Monoid
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import System.Directory (doesFileExist)
import Data.Time.Clock.POSIX (getPOSIXTime)

import Shelly (liftIO, shelly, exit, silently, run, when, unless)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Timeout
default (T.Text)

main :: IO ()
main = do
  args <- fmap T.pack <$> getArgs
  if length args < 3 then
    T.putStrLn "Usage: mpdzen.hs HOSTFILE COLOR MAXWIDTH"
  else do
    let (hostfile:color:maxw':_) = args
    ex <- liftIO $ doesFileExist $ T.unpack hostfile
    let maxw = read $ T.unpack maxw'
    ret <- timeout 500000 $ shelly $ silently $ do
      unless ex $ exit 0
      host <- head . T.lines <$> run "cat" [hostfile]
      ls <- T.lines <$> run "mpc" ["-h",host]
      when (length ls /= 3) $ exit 0

      title <- liftIO $ scroll maxw $ head ls
      let mpccmd   = "cat " <> hostfile <> " | xargs -Ihost mpc -h host "
          volume   = (filter (/="") $ concat $ T.splitOn ":" <$> (T.splitOn " " $ ls!!2))!!1
          status   = T.take 3 (ls!!1) <> "]"
          progress = (filter (not . T.null) $ T.splitOn " " (ls!!1))!!2

          fgclr = "^fg(" <> color <> ")♪"
          vol   = click "1" (mpccmd<>"volume +10")
                $ click "3" (mpccmd<>"volume -10")
                $ T.justifyRight 4 ' ' volume
          stat  = click "1" (mpccmd<>"toggle")
                $ click "3" (mpccmd<>"stop")
                $ status
          titl  = click "1" ("cat "<>hostfile<>" | xargs urxvt -e ncmpcpp -h")
                $ title
          prog  = click "1" (mpccmd<>"next")
                $ click "3" (mpccmd<>"prev")
                $ T.justifyRight 11 ' ' progress

      return $ fgclr <> vol <> " " <> stat <> " " <> titl <> " " <> prog <> " "
    T.putStrLn $ fromMaybe "" ret

click :: T.Text -> T.Text -> T.Text -> T.Text
click btn cmd str = "^ca(" <> btn <> "," <> cmd <> ")" <> str <> "^ca()"

scroll :: Int -> T.Text -> IO T.Text
scroll maxw txt = do
  time <- round <$> getPOSIXTime
  let scr = T.length txt-maxw
  return $ if scr>0 then T.take maxw $ T.drop ((time `rem` scr) - 1) txt
                    else T.center maxw ' ' txt
