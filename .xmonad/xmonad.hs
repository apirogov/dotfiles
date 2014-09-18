-- xmonad config used by Anton Pirogov
-- Copyright © 2014 Anton Pirogov
-- requires trayer + dzen2 + conky + acpi + xmonad-contrib
---------------------------------------------------------------
import System.IO (hPutStrLn)
import XMonad hiding (Tall)
import qualified XMonad.StackSet as W

import XMonad.Actions.PerWorkspaceKeys
import XMonad.Actions.UpdatePointer
import XMonad.Actions.CycleWS
import XMonad.Actions.Navigation2D

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.WallpaperSetter

import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.WorkspaceCompare (getSortByXineramaRule)
import XMonad.Util.NamedScratchpad

-- base layouts
import XMonad.Layout.HintedTile
import XMonad.Layout.Tabbed
import XMonad.Layout.Grid
import XMonad.Layout.Spiral

-- layout modifiers
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Renamed
import XMonad.Layout.Reflect
import XMonad.Layout.NoBorders
import XMonad.Layout.TrackFloating

-- layout combinators
import XMonad.Layout.IM
import XMonad.Layout.ComboP

---------------------------------------------------------------
-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
myTerminal      = "lxterminal"

-- Width of the window border in pixels.
myBorderWidth   = 2

-- Border colors for unfocused and focused windows, respectively.
myNormalBorderColor  = "#808080"
myFocusedBorderColor = "#ff8080"

-- Modifier key used for keybindings. Default is usually mod4Mask (windows key).
myModMask       = mod4Mask

-- The default number of workspaces (virtual screens) and their names.
-- The number of workspaces is determined by the length of this list.
myWorkspaces    = ["1:main","2:web","3:dev","4:docs","5:media","6:misc"]

------------------------------------------------------------------------
-- Layouts:
-- The available layouts. Each alternative layout is separated by |||,
--
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state.

myLayout = trackFloating $ avoidStruts $ smartBorders
           (   (renamed [Replace "Def"] $ defaultPerWorkspace)
           ||| (renamed [Replace "Spi"] $ mySpiral)
           ||| (renamed [Replace "Tal"] $ hintedTile Tall)
           ||| (renamed [Replace "Wid"] $ hintedTile Wide)
           ||| (renamed [Replace "Grd"] $ Grid)
           ||| (renamed [Replace "Tab"] $ tabbedLayout)
           ||| (renamed [Replace "Ful"] $ Full)
           )
  where
     mySpiral = spiral 0.618

     defaultPerWorkspace = onWorkspace "1:main"  (imLayout mySpiral)
                         $ onWorkspace "2:web"   tabbedLayout
                         $ onWorkspace "3:dev"   (hintedTile Tall)
                         $ onWorkspace "4:docs"  Grid
                         $ onWorkspace "5:media" (noBorders Full)
                         $ onWorkspace "6:misc"  Grid
                         $ hintedTile Tall       -- for all the others

     -- default for hinted Tall/Mirror Tall (HintedTile is better that layoutHints $ Tall)
     hintedTile = HintedTile nmaster delta ratio floatPos
     nmaster  = 1      -- The default number of windows in the master pane
     ratio    = 1/2 -- Default proportion of screen occupied by master pane
     delta    = 3/100  -- Percent of screen to increment by when resizing panes
     floatPos = Center -- Where floating windows spawn (TopLeft, BottomRight or Center)

     -- tabbed layout
     tabbedLayout = tabbed shrinkText myTabConfig
     -- decoration color settings for tabbed layout
     myTabConfig = defaultTheme { activeBorderColor = "#7C7C7C"
                                , activeTextColor = "#CEFFAC"
                                , activeColor = "#000000"
                                , inactiveBorderColor = "#7C7C7C"
                                , inactiveTextColor = "#EEEEEE"
                                , inactiveColor = "#000000"
                                }

     -- combined workspace layout for usage with instant messenger
     -- (keeping the Pidgin buddy list always on the right and reserving some space for the conversation window)
     imLayout n = reflectHoriz $ withIM (1/8) (ClassName "Pidgin" `And` Role "buddy_list"
                  `Or` ClassName "Gajim" `And` Role "roster"
                  `Or` ClassName "Skype" `And` (Not $ (Role "ConversationsWindow" `Or` Role "CallWindow")))
                  $ withIM (2/5) (ClassName "Pidgin" `And` Role "conversation" `Or`ClassName "Skype" `And` Role "ConversationsWindow") n

scratchpads = [
  NS "scratchpad" "urxvt -name scratchpad +sb -e bash -c 'tmux attach -t scratch || tmux new -s scratch'"
     (resource =? "scratchpad") (customFloating $ (W.RationalRect 0 0 1.0 0.5)),
  NS "neoview" "feh --title neoview ~/.xmonad/neo-druckvorlage.png"
     (title =? "neoview") (customFloating $ (W.RationalRect 0 0 1.0 0.33))
  ]

------------------------------------------------------------------------
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace. (use the xprop utility to get window attributes)
-- Also can unfloat windows. possible attributes:
-- resource/appName: first elem. of WM_CLASS
-- className: second elem. title: WM_NAME
-- others: e.g. stringProperty "WM_WINDOW_ROLE" to access it.
myManageHook = composeAll [
      className =? "Vlc"            --> doFloat
    , className =? "Pidgin"         --> doShift "1:main"
    , className =? "Skype"          --> doShift "1:main"
    , className =? "Firefox"        --> doShift "2:web"
    , className =? "Dwb"            --> doShift "2:web"
    , className =? "Thunderbird"    --> doShift "2:web"
    , className =? "Eclipse"        --> doShift "3:dev"
    , className =? "net-minecraft-LauncherFrame" --> doShift "4:media"
    , className =? "Xephyr"         --> doShift "5:media"
    , className =? "VirtualBox"     --> doShift "5:media"
    , className =? "Dolphin-emu"    --> doShift "5:media"
    , className =? "Gimp"           --> doShift "6:misc"
    ]
    <+> namedScratchpadManageHook scratchpads <+> manageDocks

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse = True

-- Perform an arbitrary action each time xmonad starts or is restarted with mod-q.
myStartupHook = spawn myTrayerCommand

-- the handle event hook to be used with xmonad
myHandleEventHook = ewmhDesktopsEventHook <+> fullscreenEventHook <+> docksEventHook

-- startup for trayer
--myTrayerCommand = "trayer --monitor primary --edge top --align right --SetDockType true --expand true --transparent true --tint 0x000000 --alpha 0 --height 16 --widthtype request"
myTrayerCommand = "trayer --monitor primary --edge top --align right --SetDockType true --expand true --transparent true --tint 0x000000 --alpha 0 --height 16 --widthtype pixel --width 100"

-- command line calls to my dzen2 instances:
-- use DZen2 version with Xinerama, XFT and XPM (Option 7 in config.mk)
myDZenXMBar = "dzen2 -e '' -xs 1 -h 16 -w 500 -fn 'Inconsolata:size=10' -ta l"
myDZenSTBar = "conky -c ~/.xmonad/conkyrc | dzen2 -e '' -fn 'Inconsolata:size=10' -xs 1 -ta r -x 500 -h 16 -w 1000"

-- log hook for usage with dzen2
myDzenLogHook h = dynamicLogWithPP $ defaultPP {
                    ppOutput  = hPutStrLn h
                  , ppCurrent = dzenColor "#ffffff" "#404040" . pad
                  , ppVisible = dzenColor "#ffffff" "" . pad
                  , ppHidden  = dzenColor "#808080" "" . pad
                  , ppUrgent  = dzenColor "#ffff00" "" . pad . dzenStrip
                  , ppHiddenNoWindows = (\x -> "")
                  , ppWsSep   = ""
                  , ppSep     = " "
                  , ppTitle   = dzenColor "#ffffff" ""
                  , ppLayout  = (\x -> dzenColor (case x of
                                  "Def" -> "#800080"
                                  "Spi" -> "#2020b0"
                                  "Tal" -> "#008080"
                                  "Wid" -> "#00b000"
                                  "Grd" -> "#b0b000"
                                  "Tab" -> "#b08000"
                                  "Ful" -> "#b00000"
                                  _     -> "#b0b0b0"
                                ) "" x)
                  , ppSort    = fmap (.namedScratchpadFilterOutWorkspace) getSortByXineramaRule
                  }

---------------------------------------------------------------

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults = defaultConfig {
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

-- If Layout has the description string s, execute f, else g
onLayout :: String -> X () -> X () -> X ()
onLayout s f g = do
  XState { windowset = wset} <- get
  let ws = head $ filter (\(W.Workspace i _ _) -> i==W.currentTag wset) $ W.workspaces wset
  if (description . W.layout) ws == s then f else g

-- Hack to make WindowNavigation2D work with my tabbed layouts
onTabbed f g = bindOn [ ("2:web", onDefTab),("", onTab) ]
  where onDefTab = onLayout "Def" f onTab
        onTab    = onLayout "Tab" f g

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
--
mediahost = "mediacenter"
main = do
  rightBar <- spawnPipe myDZenSTBar
  bar <- spawnPipe myDZenXMBar

  xmonad $ withNavigation2DConfig defaultNavigation2DConfig {
             layoutNavigation   = [("Full", centerNavigation)]
           , unmappedWindowRect = [("Full", singleWindowRect)]
         } $ withUrgencyHook NoUrgencyHook $ ewmh $ defaults {
    logHook = do
                setWMName "LG3D"
                ewmhDesktopsLogHook
                myDzenLogHook bar
                updatePointer (Relative 0.5 0.5)
                wallpaperSetter defWallpaperConf {
                    wallpapers=defWPNames myWorkspaces
                                          `modWPList` [("1:main",WallpaperDir "random")] }

  } `additionalKeys` [  -- Key bindings --
    -- override default xmonad restart binding (to kill dzen2)
      ((myModMask, xK_q), spawn "killall trayer conky dzen2; xmonad --recompile && xmonad --restart")
    -- Screenshot
    , ((0, xK_Print), spawn "scrot -q 95 %Y-%m-%d_%H%M%S.jpg")
    -- change keyboard layout
    , ((0, xK_Scroll_Lock), spawn "setxkbmap -v | grep 'us('; if [[ \"$?\" == '0' ]]; then setxkbmap de neo -option; else setxkbmap us cz_sk_de -option -option caps:escape; fi")

    -- Easy navigation, binding with Neo2 layer 4 arrows
    , ((controlMask .|. mod3Mask, xK_e), onTabbed (windows W.focusDown) (windowGo R True))
    , ((controlMask .|. mod3Mask, xK_i), onTabbed (windows W.focusUp) (windowGo L True))
    , ((controlMask .|. mod3Mask, xK_l), windowGo U True)
    , ((controlMask .|. mod3Mask, xK_a), windowGo D True)
    , ((myModMask .|. mod3Mask,  xK_e), onTabbed (windows W.swapDown) (windowSwap R True))
    , ((myModMask .|. mod3Mask,  xK_i), onTabbed (windows W.swapUp) (windowSwap L True))
    , ((myModMask .|. mod3Mask,  xK_l), windowSwap U True)
    , ((myModMask .|. mod3Mask,  xK_a), windowSwap D True)
    -- CycleWS
    , ((myModMask,               xK_z), toggleWS)
    , ((myModMask,               xK_w), prevScreen)
    , ((myModMask,               xK_e), nextScreen)
    , ((myModMask .|. shiftMask, xK_w), shiftPrevScreen)
    , ((myModMask .|. shiftMask, xK_e), shiftNextScreen)
    -- toggle dock spacing
    , ((myModMask, xK_b), sendMessage ToggleStruts)
    -- scratchpad
    , ((myModMask, xK_BackSpace), namedScratchpadAction scratchpads "scratchpad")
    , ((myModMask, xK_Delete), windows W.focusDown <+> namedScratchpadAction scratchpads "neoview" <+> windows W.focusUp)

    -- Fn Media keys which do not work automatically
    -- , ((0, 0x1008ff18), spawn "echo not set")            -- Home key
    , ((0, 0x1008ff41), spawn "synclient TouchpadOff=$(synclient -l | grep -c 'TouchpadOff.*=.*0')") -- black launch key Thinkvantage
    , ((0, 0x1008ff2d), spawn "xscreensaver-command -lock") -- lock key on thinkpad
    , ((0, 0xff25),     spawn "xscreensaver-command -lock") -- Fn-Esc on truly
    , ((0, 0x1008ff2f), spawn "sudo systemctl suspend")     -- suspend media key
    , ((0, 0x1008ff8f), spawn "skype")                      -- key with headphones and camera
    , ((0, 0x1008ff59), spawn "screens")                    -- key with projector

    -- Volume keys
    , ((0, 0x1008ff12), spawn "amixer set Master toggle")  -- speaker mute
    , ((0, 0x1008ff11), spawn "amixer set Master 4%-")     -- volume up + down
    , ((0, 0x1008ff13), spawn "amixer set Master 4%+")
    , ((0, 0x1008ffb2), spawn "amixer set Capture toggle") -- mic mute

    -- Prev/Next/Pause keys
    , ((0, 0x1008ff14), spawn "mpc toggle")
    , ((0, 0x1008ff17), spawn "mpc next")
    , ((0, 0x1008ff16), spawn "mpc prev")
    -- MPD specific volume
    , ((myModMask, 0x1008ff11), spawn "mpc volume -10")
    , ((myModMask, 0x1008ff13), spawn "mpc volume +10")
    -- For media center music control
    , ((controlMask, 0x1008ff14), spawn $ "mpc -h $(cat .mpdpwd)@" ++ mediahost ++ " toggle")
    , ((controlMask, 0x1008ff17), spawn $ "mpc -h $(cat .mpdpwd)@" ++ mediahost ++ " next")
    , ((controlMask, 0x1008ff16), spawn $ "mpc -h $(cat .mpdpwd)@" ++ mediahost ++ " prev")
    , ((controlMask, 0x1008ff11), spawn $ "mpc -h $(cat .mpdpwd)@" ++ mediahost ++  " volume -10")
    , ((controlMask, 0x1008ff13), spawn $ "mpc -h $(cat .mpdpwd)@" ++ mediahost ++  " volume +10")
    ]

