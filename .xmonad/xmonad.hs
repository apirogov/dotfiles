{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses #-}
-- xmonad config of Anton Pirogov
-- requires trayer-srg-git + dzen2 + xdotool + conky + xmonad-contrib-darcs
---------------------------------------------------------------
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Control.Applicative ((<$>))
import Control.Monad (when)

import XMonad
import qualified XMonad.StackSet as W
import Graphics.X11.ExtraTypes.XF86
import System.Exit (exitSuccess)

import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.PerWorkspaceKeys
import XMonad.Actions.CycleWS
import XMonad.Actions.MouseResize
import XMonad.Actions.Navigation2D
import XMonad.Actions.RotSlaves (rotAllUp,rotAllDown)
import XMonad.Actions.UpdatePointer (updatePointer)

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.WallpaperSetter

import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (spawnPipe, hPutStrLn, runProcessWithInput, runInTerm)
import XMonad.Util.WorkspaceCompare (getSortByXineramaRule,getSortByIndex)

import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Input

-- base layouts
import XMonad.Layout.Tabbed
import BinarySpacePartitionMod
-- layout modifiers
import XMonad.Layout.BorderResize
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.Layout.TrackFloating
import XMonad.Layout.WindowSwitcherDecoration
import XMonad.Layout.NoBorders (smartBorders,noBorders)
-- layout combinators
import XMonad.Layout.IM
import XMonad.Layout.MultiToggle
import XMonad.Layout.Reflect (REFLECTX(..),REFLECTY(..))

---------------------------------------------------------------
-- The preferred terminal program, which is used in a binding below and by certain contrib modules.
myTerminal           = "lxterminal"
-- Whether focus follows the mouse pointer.
myFocusFollowsMouse  = True
-- Width of the window border in pixels.
myBorderWidth        = 2
-- Border colors for unfocused and focused windows, respectively.
myNormalBorderColor  = "#808080"
myFocusedBorderColor = "#ff8080"
-- Modifier key used for keybindings. Default is usually mod4Mask (windows key).
-- 1 = left alt, 2 = Numlock, 3 = Capslock, 4 = Super key
myModMask            = mod4Mask
-- The default number of workspaces (virtual screens) and their names.
myWorkspaces         = ["1","2"]

------------------------------------------------------------------------
-- custom transformer
data CUSTOM = TABBED | NBFULL deriving (Show, Read, Eq, Typeable)
instance Transformer CUSTOM Window where
  transform NBFULL  x k = k fullLayout    $ const x
  transform TABBED  x k = k tabbedLayout  $ const x

fullLayout    = renamed [Replace "Ful"] $ noBorders Full
tabbedLayout  = renamed [Replace "Tab"] $ tabbed shrinkText myTabConfig
  where myTabConfig  = def {
                              activeBorderColor = "#7C7C7C" , inactiveBorderColor = "#7C7C7C"
                            , activeTextColor   = "#CEFFAC" , inactiveTextColor   = "#EEEEEE"
                            , activeColor       = "#000000" , inactiveColor       = "#000000"
                            }

-- my main layout with the modifier and toggle stack
myLayout = trackFloating $ smartBorders $ avoidStruts $ mkToggle1 NBFULL $ mkToggle1 TABBED
         $ renamed [Replace "Bsp"] $ winSwitcher $ borderResize $ spacingWithEdge 5
         $ mkToggle1 REFLECTX $ mkToggle1 REFLECTY $ imLayout $ emptyBSP
  where winSwitcher = renamed [CutWordsLeft 1] . windowSwitcherDecoration
                                                   shrinkText def{activeColor="#002590"}
        imLayout    = (renamed [CutWordsLeft 1]) . (withIM (1/8)
                      (ClassName "Pidgin" `And` Role "buddy_list"
                      `Or` ClassName "Gajim" `And` Role "roster"
                      `Or` ClassName "Skype" `And`
                        (Not $ (Role "ConversationsWindow" `Or` Role "CallWindow"))))

------------------------------------------------------------------------
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace. (use the xprop utility to get window attributes)
-- Also can unfloat windows. possible attributes:
-- resource/appName: first elem. of WM_CLASS
-- className: second elem. title: WM_NAME
-- others: e.g. stringProperty "WM_WINDOW_ROLE" to access it.
myManageHook = (isFullscreen --> doFullFloat)
            <+> namedScratchpadManageHook scratchpads <+> manageDocks

-- Perform an arbitrary action each time xmonad starts or is restarted with mod-q.
myStartupHook = spawn myTrayerCommand

-- the handle event hook to be used with xmonad
myHandleEventHook = ewmhDesktopsEventHook <+> fullscreenEventHook <+> docksEventHook
---------------------------------------------------------------

myConf = def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
        modMask            = myModMask,
        workspaces         = myWorkspaces,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        startupHook        = myStartupHook,
        handleEventHook    = myHandleEventHook
    }

-- command line calls to my dzen2 and trayer instances
-- use DZen2 version with Xinerama, XFT and XPM (Option 7 in config.mk)
myDzenStatus = "dzen2 -ta l -w 500" ++ myDzenStyle
myDzenConky  = "conky -c ~/.xmonad/conkyrc | dzen2 -ta r -x 500 -w 1000" ++ myDzenStyle
myDzenStyle = " -e 'onstart=lower' -fn 'Inconsolata:size=10' -xs 1 -h 16"
myTrayerCommand = "trayer -l --monitor primary --edge top --align right --expand true "
                ++"--transparent true --tint 0x000000 --alpha 0 --height 16 --widthtype pixel --width 100"

-- log hook for usage with dzen2
myDzenLogHook h = dynamicLogWithPP $ def {
                    ppOutput  = hPutStrLn h
                  , ppCurrent = dzenColor "#ffffff" "#404040" . pad
                  , ppVisible = dzenColor "#ffffff" "" . pad . clickWS
                  , ppHidden  = dzenColor "#808080" "" . pad . clickWS
                  , ppUrgent  = dzenColor "#ffff00" "" . pad . clickWS . dzenStrip
                  , ppHiddenNoWindows = const ""
                  , ppWsSep   = ""
                  , ppSep     = " "
                  , ppTitle   = dzenColor "#ffffff" ""
                  , ppLayout  = (\x -> click 1 "xdotool key super+space" $ dzenColor (getWSColor x) "" x)
                  , ppSort    = (.namedScratchpadFilterOutWorkspace) <$> getSortByXineramaRule
                  }
  where click btn cmd str = "^ca("++(show btn)++","++cmd++")"++str++"^ca()"
        clickWS name = click 1 ("xdotool key super+"++(take 1 name)) name
        rainbow = ["#b0b0b0","#b000b0","#4040ff","#00d0d0","#00f000","#f0f000","#ffa000","#ff0000"]
        getWSColor x = (rainbow !!) $ (1+) $ fromMaybe (-1) (x `elemIndex` ["Bsp","Ful","Tab","Flt"])

-- If Layout has the description string s, execute f, else g
onLayout :: String -> X () -> X () -> X ()
onLayout s f g = do
  XState { windowset = wset} <- get
  let ws = head $ filter (\(W.Workspace i _ _) -> i==W.currentTag wset) $ W.workspaces wset
  if (description . W.layout) ws == s then f else g
-- Hack to make WindowNavigation2D work with my tabbed layouts
onTabbed = onLayout "Tab"

-- make focused window floating (reverse of unfloat)
floatWindow = (W.stack . W.workspace . W.current) <$> gets windowset >>= flip whenJust (float . W.focus)

-- for dynamic workspaces: do action to workspace by name. if not existing, create, then do
withNamedWorkspace job str = do
  ws <- gets windowset
  sort <- getSortByIndex
  -- io $ spawn $ "notify-send "++str
  case str `elemIndex` (map W.tag $ sort $ W.workspaces ws) of
    Just i -> withNthWorkspace job i
    Nothing -> addHiddenWorkspace str >> withNamedWorkspace job str

-- dmenu-like colors
myXPConfig = def {
    position = Top , promptBorderWidth = 0
  , bgColor = "#202020" , fgColor = "#d0d0d0"
  , bgHLight = "#004080" , fgHLight = "#f0f0f0"
  }

-- confirm action before performing
confirm :: String -> X () -> X ()
confirm msg f = do
    ret <- inputPromptWithCompl myXPConfig msg $ mkComplFunFromList ["y","n"]
    whenJust ret $ \s -> when (s=="y") f

-- my universal prompt - overloaded shell prompt with nested sub-prompts
myPrompt = do
  cmds <- io getCommands
  ret <- inputPromptWithCompl myXPConfig "" $ myCompl (cmds++map fst prompthooks)
  whenJust ret $ exec
  where myCompl = getShellCompl
        exec s
          | Just h <- lookup p prompthooks = h -- $ if null r then "" else tail r
          | s `elem` tp = runInTerm "" s
          | otherwise   = spawn s
          where (p,r) = break (==':') s
        tp = ["ranger","ncmpcpp","ssh","vim"] -- programs to run in terminal
        prompthooks = [("n",netctlP)          -- commands calling sub-prompts
                      ,("c",calcP)]

-- prompt to select netctl config to connect to (first disconnects all)
netctlP = do
  nets <- lines <$> runProcessWithInput "bash"
                   ["-c", "find /etc/netctl/ -maxdepth 1 -type f | sed 's/^.*\\///'"] ""
  ret <- inputPromptWithCompl myXPConfig "netctl" $ compl ("-":nets)
  whenJust ret $ \n -> spawn $ "sudo netctl stop-all && sudo netctl start "++n
  where compl nets "" = return nets --show all networks on startup
        compl nets s = (mkComplFunFromList nets) s

-- use ghc as calculator
calcP = do
  ret <- inputPrompt myXPConfig "calc"
  whenJust ret $ \n -> spawn $ "bash -c \"notify-send $(ghc -e '"++n++"')\""

scratchpads = [
  NS "scratchpad" "urxvt -name scratchpad +sb -e bash -c 'tmux attach -t scratch || tmux new -s scratch'"
     (resource =? "scratchpad") (customFloating $ (W.RationalRect 0 0 1.0 0.5)),
  NS "neoview" "feh --title neoview ~/.xmonad/neo-druckvorlage.png"
     (title =? "neoview") (customFloating $ (W.RationalRect 0 0 1.0 0.33))
  ]

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
main = do
  status <- spawnPipe myDzenStatus
  conky  <- spawnPipe myDzenConky

  xmonad $ withNavigation2DConfig def { defaultTiledNavigation = centerNavigation }
         $ withUrgencyHook NoUrgencyHook $ ewmh $ myConf {
    logHook = do
                setWMName "LG3D"
                ewmhDesktopsLogHook
                updatePointer (0.5, 0.5) (0,0)
                myDzenLogHook status
                wallpaperSetter def { wallpapers = defWPNames (map show [0..9])
                                        <> WallpaperList [("1",WallpaperDir "random")] }

  } `additionalKeys` ([  -- Key bindings --
      ((myModMask, xK_q), spawn "xmonad --recompile && killall trayer conky dzen2; xmonad --restart")
    , ((myModMask .|. shiftMask,  xK_q), confirm "Exit?" $ io exitSuccess)
    , ((myModMask, xK_p), myPrompt)
    , ((myModMask .|. shiftMask,  xK_t), floatWindow)

    -- Navigation2D, binding with Neo2 layer 4 arrows
    , ((controlMask .|. mod3Mask, xK_e), onTabbed (windows W.focusDown) (windowGo R True))
    , ((controlMask .|. mod3Mask, xK_i), onTabbed (windows W.focusUp) (windowGo L True))
    , ((controlMask .|. mod3Mask, xK_l), windowGo U True)
    , ((controlMask .|. mod3Mask, xK_a), windowGo D True)
    , ((myModMask .|. mod3Mask,   xK_e), onTabbed (windows W.swapDown) (windowSwap R True))
    , ((myModMask .|. mod3Mask,   xK_i), onTabbed (windows W.swapUp) (windowSwap L True))
    , ((myModMask .|. mod3Mask,   xK_l), windowSwap U True)
    , ((myModMask .|. mod3Mask,   xK_a), windowSwap D True)
    , ((myModMask,                xK_z), switchLayer)

    -- CycleWS for screen handling
    , ((myModMask,                xK_w), prevScreen)
    , ((myModMask,                xK_e), nextScreen)
    , ((myModMask .|. shiftMask,  xK_w), shiftPrevScreen)
    , ((myModMask .|. shiftMask,  xK_e), shiftNextScreen)

    -- toggle modifiers
    , ((myModMask,                xK_y), sendMessage ToggleStruts)
    , ((myModMask,                xK_x), sendMessage $ Toggle NBFULL)
    , ((myModMask .|. shiftMask,  xK_x), sendMessage $ Toggle TABBED)
    , ((myModMask,                xK_v), sendMessage $ Toggle REFLECTY)
    , ((myModMask .|. shiftMask,  xK_v), sendMessage $ Toggle REFLECTX)

    -- scratchpad
    , ((myModMask, xK_BackSpace), namedScratchpadAction scratchpads "scratchpad")
    , ((myModMask, xK_Delete), windows W.focusDown <+> namedScratchpadAction scratchpads "neoview" <+> windows W.focusUp)

    -- rotate windows (BSPWM-like circulat)
    , ((myModMask,               xK_f),     rotAllUp)
    , ((myModMask.|. shiftMask,  xK_f),     rotAllDown)

    -- window spacing (eye candy)
    , ((myModMask,               xK_g),     incSpacing 2)
    , ((myModMask .|. shiftMask, xK_g),     incSpacing (-2))

    -- Binary Space Partition layout control
    , ((myModMask,               xK_Right), sendMessage $ ExpandTowards R)
    , ((myModMask,               xK_Left),  sendMessage $ ExpandTowards L)
    , ((myModMask,               xK_Up),    sendMessage $ ExpandTowards U)
    , ((myModMask,               xK_Down),  sendMessage $ ExpandTowards D)
    , ((myModMask .|. shiftMask, xK_Right), sendMessage $ ShrinkFrom R)
    , ((myModMask .|. shiftMask, xK_Left),  sendMessage $ ShrinkFrom L)
    , ((myModMask .|. shiftMask, xK_Up),    sendMessage $ ShrinkFrom U)
    , ((myModMask .|. shiftMask, xK_Down),  sendMessage $ ShrinkFrom D)
    , ((myModMask,               xK_r),     sendMessage Rotate)
    , ((myModMask,               xK_s),     sendMessage Swap)
    , ((myModMask,               xK_b),     sendMessage Balance)
    , ((myModMask .|. shiftMask, xK_b),     sendMessage Equalize)
    , ((myModMask,               xK_n),     sendMessage FocusParent)

    -- media keys which do not work automatically
    , ((0, xK_Print), spawn "scrot -q 95 %Y-%m-%d_%H%M%S.jpg") -- Screenshot
    , ((0, xK_Scroll_Lock), spawn togKBLayout)              -- ScrLck on Thinkpad L530 = Fn+K
    , ((0, 0x1008ff41), spawn togTouchpad)                  -- black launch key Thinkvantage
    , ((0, 0x1008ff02), spawn "xbacklight -inc 10")
    , ((0, 0x1008ff03), spawn "xbacklight -dec 10")
    , ((0, 0x1008ff2d), spawn "xscreensaver-command -lock") -- lock key on thinkpad
    , ((0, 0xff25),     spawn "xscreensaver-command -lock") -- Fn-Esc on my TECK
    -- , ((0, 0x1008ff2f), spawn "sudo systemctl suspend")     -- suspend media key (systemd takes care (logind.conf))
    , ((0, 0x1008ff59), spawn "screens")                    -- key with projector

    -- system volume keys
    , ((0, 0x1008ff12), spawn "amixer set Master toggle")  -- speaker mute
    , ((0, 0x1008ff11), spawn "amixer set Master 4%-")     -- volume up + down
    , ((0, 0x1008ff13), spawn "amixer set Master 4%+")
    , ((0, 0x1008ffb2), spawn "amixer set Capture toggle") -- mic mute

    -- prev/next/pause/volume for local MPD
    , ((0, 0x1008ff14), spawn "mpc toggle")
    , ((0, 0x1008ff17), spawn "mpc next")
    , ((0, 0x1008ff16), spawn "mpc prev")
    , ((myModMask, 0x1008ff11), spawn "mpc volume -10")
    , ((myModMask, 0x1008ff13), spawn "mpc volume +10")
    -- For media center music control
    , ((controlMask, 0x1008ff14), spawn $ mpccmd ++ " toggle")
    , ((controlMask, 0x1008ff17), spawn $ mpccmd ++ " next")
    , ((controlMask, 0x1008ff16), spawn $ mpccmd ++ " prev")
    , ((controlMask, 0x1008ff11), spawn $ mpccmd ++ " volume -10")
    , ((controlMask, 0x1008ff13), spawn $ mpccmd ++ " volume +10")
    ]
    ++ -- dynamic workspaces for all unused numbers (will be auto-added + auto-removed when empty)
    (drop (length myWorkspaces) $ zip (zip (repeat myModMask) $ [xK_1..xK_9]++[xK_0])
        (map (\ws->removeEmptyWorkspaceAfterExcept myWorkspaces (return ())>>addWorkspace ws)
             $ map show $ [1..9]++[0]))
    ++
    (drop (length myWorkspaces) $ zip (zip (repeat (myModMask .|. shiftMask)) $ [xK_1..xK_9]++[xK_0])
        (map (withNamedWorkspace W.shift) $ map show $ [1..9]++[0]))
    )

    where mpccmd = "mpc -h $(head -n 1 .mpdpwd)" -- .mpdpwd contains PASSWORD@HOSTNAME
          togTouchpad = "synclient TouchpadOff=$(synclient -l | grep -c 'TouchpadOff.*=.*0')"
          togKBLayout = "setxkbmap -v | grep 'us('; if [[ \"$?\" == '0' ]]; then setxkbmap de neo -option;"
                       ++ " else setxkbmap us cz_sk_de -option -option caps:escape; fi"

