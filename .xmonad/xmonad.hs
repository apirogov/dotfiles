-- xmonad config used by Anton Pirogov
-- Copyright © 2012 Anton Pirogov
-- requires fully enabled dzen2 or MPD enabled xmobar + conky + acpi + xmonad-contrib
---------------------------------------------------------------

import System.IO
import System.Exit
import XMonad hiding (Tall)

-- actions
import XMonad.Actions.PerWorkspaceKeys
import XMonad.Actions.GridSelect

-- hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops

-- base layouts
import XMonad.Layout.HintedTile
import XMonad.Layout.Tabbed
import XMonad.Layout.Grid
import XMonad.Layout.Spiral
import XMonad.Layout.Circle

-- layout modifiers
import XMonad.Layout.PerWorkspace
import XMonad.Layout.WindowNavigation
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.Reflect

-- layout combinators
import XMonad.Layout.IM
import XMonad.Layout.ComboP
import XMonad.Layout.TwoPane

-- other stuff
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeys)
-- import XMonad.Util.WorkspaceCompare

---------------------------------------------------------------

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
myTerminal      = "lxterminal"

-- Width of the window border in pixels.
myBorderWidth   = 2

-- Border colors for unfocused and focused windows, respectively.
myNormalBorderColor  = "#7c7c7c"
myFocusedBorderColor = "#ffb6b0"

-- Modifier key used for keybindings. Default is usually mod4Mask (windows key).
myModMask       = mod4Mask

-- The default number of workspaces (virtual screens) and their names.
-- The number of workspaces is determined by the length of this list.
myWorkspaces    = ["1:main","2:web","3:dev","4:media","5:misc"]

------------------------------------------------------------------------
-- Layouts:
-- The available layouts. Each alternative layout is separated by |||,
--
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state.

myLayout = windowNavigation $ avoidStruts
           ( defaultPerWorkspace ||| hintedTile Tall ||| hintedTile Wide ||| tabbedLayout ||| Full ||| Grid ||| spiral (6/7) ||| Circle )
  where
     defaultPerWorkspace = named "Default"
                         $ onWorkspace "1:main"  pidginLayout
                         $ onWorkspace "2:web"   tabbedLayout
                         $ onWorkspace "3:dev"   codingLayout
                         $ onWorkspace "4:media" Full
                         $ onWorkspace "5:misc"  Grid
                         $ hintedTile Tall       -- for all the others

     -- default for hinted Tall/Mirror Tall (HintedTile is better that layoutHints $ Tall)
     hintedTile = HintedTile nmaster delta ratio floatPos
     nmaster  = 1      -- The default number of windows in the master pane
     ratio    = 1/2    -- Default proportion of screen occupied by master pane
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
     pidginLayout = reflectHoriz $ withIM (1/8) (ClassName "Pidgin" `And` Role "buddy_list")
                                 $ withIM (1/3) (ClassName "Pidgin" `And` Role "conversation") (hintedTile Wide)

     -- console coding layout
     codingLayout = combineTwoP (Mirror $ TwoPane (3/100) (2/3))
                                (tabbedLayout) (hintedTile Tall)
                                (ClassName "Gvim" `Or` ClassName "Eclipse")

------------------------------------------------------------------------
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace. (use the xprop utility to get window attributes)
myManageHook = composeAll [
      className =? "Vlc"            --> doFloat
    , className =? "Pidgin"         --> doShift "1:main"
    , className =? "Firefox"        --> doShift "2:web"
    , className =? "Eclipse"        --> doShift "3:dev"
    , className =? "net-minecraft-LauncherFrame" --> doShift "4:media"
    , className =? "Gimp"           --> doShift "5:misc"
    ]

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Perform an arbitrary action each time xmonad starts or is restarted with mod-q.
myStartupHook = do
--    spawn "firefox"
    return ()

-- the handle event hook to be used with xmonad
myHandleEventHook = ewmhDesktopsEventHook <+> docksEventHook

-- log hook for usage with dzen2
myDzenLogHook h = dynamicLogWithPP $ dzenPP { ppOutput = hPutStrLn h }

-- log hook to be used with XMobar (pass a pipe to a running xmobar)
myXmobarLogHook h =  do
                      setWMName "LG3D"
                      ewmhDesktopsLogHook
                      dynamicLogWithPP $ xmobarPP {
                            ppOutput = hPutStrLn h
                          , ppTitle = xmobarColor "#FFB6B0" "" . shorten 100
                          , ppCurrent = xmobarColor "#CEFFAC" ""
                          , ppSep = "   "
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
        layoutHook         = smartBorders $ myLayout,
        manageHook         = myManageHook <+> manageDocks,
        startupHook        = myStartupHook,
        handleEventHook    = myHandleEventHook
    }

-- which status bar to use - True=Dzen2 False=XMobar
useDZen = True

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
--
-- command line calls to my dzen2 instances:
-- when using DZen2, use version with Xinerama, XFT and XPM (Option 7 in config.mk)
myDZenXMBar = "dzen2 -xs 1 -h 16 -w 550 -fn 'Monaco:size=8' -ta l"
myDZenSTBar = "conky | dzen2 -fn 'Monaco:size=8' -xs 1 -ta r -x 550 -h 15 -w 950"
mediahost = "mediacenter"
main = do
  rightBar <- if useDZen
                then spawnPipe myDZenSTBar
                else spawnPipe ""
  bar <- if useDZen
            then spawnPipe myDZenXMBar
            else spawnPipe $ "MPD_HOST="++ mediahost ++" ~/.cabal/bin/xmobar ~/.xmonad/xmobar"

  xmonad $ ewmh $ defaults {
    --logHook = myDzenLogHook dzenBar
    logHook = if useDZen
                 then myDzenLogHook bar
                 else myXmobarLogHook bar
	} `additionalKeys` ( [  -- Key bindings --
    -- improved WindowNavigation keybindings
      ((myModMask,               xK_Right), sendMessage $ Go R)
    , ((myModMask,               xK_Left ), sendMessage $ Go L)
    , ((myModMask,               xK_Up   ), sendMessage $ Go U)
    , ((myModMask,               xK_Down ), sendMessage $ Go D)
    , ((myModMask .|. shiftMask, xK_Right), sendMessage $ Swap R)
    , ((myModMask .|. shiftMask, xK_Left ), sendMessage $ Swap L)
    , ((myModMask .|. shiftMask, xK_Up   ), sendMessage $ Swap U)
    , ((myModMask .|. shiftMask, xK_Down ), sendMessage $ Swap D)
    , ((myModMask .|. shiftMask, xK_s    ), sendMessage $ SwapWindow)  --Swap between panes in ComboP TwoPane
    -- Screenshot
    , ((0, xK_Print), spawn "scrot -q 95 %Y-%m-%d_%H%M%S.jpg")
    -- Home key
    -- , ((0, 0x1008ff18), spawn "echo not set")
    -- Audio keys
    , ((0, 0x1008ff12), spawn "amixer set Master toggle")
    , ((0, 0x1008ff11), spawn "amixer set Master 4%-")
    , ((0, 0x1008ff13), spawn "amixer set Master 4%+")
    , ((0, 0x1008ff41), spawn "firefox") --black launch key
    --, ((0, ???), spawn "amixer set Capture toggle") --mute key
    -- , ((0, keycode 114), spawn "mkdir test")
    -- Fn Media keys
    , ((0, 0x1008ff2d), spawn "xscreensaver-command -lock")
    , ((0, 0x1008ff8f), spawn "skype")
    , ((0, 0x1008ff59), spawn "screens")
    , ((0, 0x1008ff14), spawn "mpc toggle")
    , ((0, 0x1008ff17), spawn "mpc next")
    , ((0, 0x1008ff16), spawn "mpc prev")
    -- MPD specific hotkeys
    , ((myModMask .|. controlMask, xK_plus), spawn "mpc volume +10")
    , ((myModMask .|. controlMask, xK_minus), spawn "mpc volume -10")
    -- For media center music control
    , ((controlMask, 0x1008ff14), spawn $ "mpc -h " ++ mediahost ++ " toggle")
    , ((controlMask, 0x1008ff17), spawn $ "mpc -h " ++ mediahost ++ " next")
    , ((controlMask, 0x1008ff16), spawn $ "mpc -h " ++ mediahost ++ " prev")
    , ((myModMask .|. controlMask, xK_Prior), spawn $ "mpc -h " ++ mediahost ++  " volume +10")
    , ((myModMask .|. controlMask, xK_Next), spawn $ "mpc -h " ++ mediahost ++  " volume -10")
    -- per workspace keys
    , ((myModMask .|. controlMask, xK_r), bindOn [
         ("3:dev", do spawn "pcmanfm"
                      spawn myTerminal
                      spawn $ myTerminal ++ " -t repl -e 'tmux new-session -s repl -n repl'"
                      spawn "gvim")
        ,("",       spawn "notify-send \"not specified\"")
        ])
    -- grid selector
    , ((myModMask, xK_g), goToSelected defaultGSConfig)
    -- override default xmonad restart binding (to kill dzen2)
    , ((myModMask, xK_q), spawn "killall conky dzen2 && xmonad --recompile && xmonad --restart")
    ])

