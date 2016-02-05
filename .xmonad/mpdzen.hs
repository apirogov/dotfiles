{-# LANGUAGE OverloadedStrings #-}
import System.Environment (getArgs)
import System.Directory (doesFileExist)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Monoid
import Shelly
import qualified Data.Text as T

click btn cmd str = "^ca(" <> btn <> "," <> cmd <> ")" <> str <> "^ca()"

main = do
  (hostfile:color:maxw':_) <- fmap T.pack <$> getArgs
  ex <- liftIO $ doesFileExist $ T.unpack hostfile
  let maxw = read $ T.unpack maxw'
  shelly $ silently $ do
    unless ex $ exit 0
    host <- head . T.lines <$> run "cat" [hostfile]
    ls <- T.lines <$> run "mpc" ["-h",host]
    when (length ls /= 3) $ exit 0

    title <- liftIO $ scroll maxw $ ls!!0
    let mpccmd   = "cat " <> hostfile <> " | xargs -Ihost mpc -h host "
        volume   = (filter (/="") $ concat $ T.splitOn ":" <$> (T.splitOn " " $ ls!!2))!!1
        status   = T.take 3 (ls!!1) <> "]"
        progress = (T.splitOn " " (ls!!1))!!4
    
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

    echo $ fgclr <> vol <> " " <> stat <> " " <> titl <> " " <> prog <> " "

scroll :: Int -> T.Text -> IO T.Text
scroll maxw txt = do
  time <- round <$> getPOSIXTime
  let scr = T.length txt-maxw
  return $ if scr>0 then T.take maxw $ T.drop ((time `rem` scr) - 1) txt
                    else T.center maxw ' ' txt
