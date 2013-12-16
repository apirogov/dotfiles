-- xmonad config used by Anton Pirogov
-- Copyright © 2013 Anton Pirogov
-- modWorkspace sucks in 0.11. Use darcs version/0.12
-- requires fully enabled dzen2 or MPD enabled xmobar + conky + acpi + xmonad-contrib
---------------------------------------------------------------

import System.IO (hPutStrLn)
import XMonad hiding (Tall)
import qualified XMonad.StackSet as W
import qualified Data.Map as M

-- actions
import XMonad.Actions.PerWorkspaceKeys
import XMonad.Actions.GridSelect
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.UpdatePointer

-- hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.UrgencyHook

-- base layouts
import XMonad.Layout.LayoutHints (layoutHints)
import XMonad.Layout.HintedTile
import XMonad.Layout.Tabbed
import XMonad.Layout.Grid
import XMonad.Layout.Spiral

-- layout modifiers
import XMonad.Layout.PerWorkspace
import XMonad.Layout.WindowNavigation
import XMonad.Layout.Named
import XMonad.Layout.Reflect
import XMonad.Layout.NoBorders

-- layout combinators
import XMonad.Layout.IM
import XMonad.Layout.ComboP
import XMonad.Layout.Combo
import XMonad.Layout.TwoPane

-- other stuff
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.WorkspaceCompare (getSortByXineramaRule)
import XMonad.Util.Scratchpad
import XMonad.Util.Replace
import XMonad.Prompt (defaultXPConfig)

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

myLayout = avoidStruts $ smartBorders $ windowNavigation $ {-- modWorkspace "1:main" imLayout --}
           (   (named "Def" $ defaultPerWorkspace)
           ||| (named "Spi" $ mySpiral)
           ||| (named "Tal" $ hintedTile Tall)
           ||| (named "Wid" $ hintedTile Wide)
           ||| (named "Grd" $ Grid)
           ||| (named "Tab" $ tabbedLayout)
           ||| (named "Ful" $ Full)
           )
  where
     mySpiral = spiral 0.618

     defaultPerWorkspace = named "Def"
                         $ onWorkspace "1:main"  mySpiral
                         $ onWorkspace "2:web"   tabbedLayout
                         $ onWorkspace "3:dev"   tabbedLayout
                         $ onWorkspace "4:docs"  mySpiral
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
     imLayout n = nameTail $ nameTail $ nameTail
                  $ reflectHoriz $ withIM (1/8) (ClassName "Pidgin" `And` Role "buddy_list"
                  `Or` ClassName "Gajim" `And` Role "roster"
                  `Or` ClassName "Skype" `And` (Not $ (Role "ConversationsWindow" `Or` Role "CallWindow")))
                  $ withIM (2/5) (ClassName "Pidgin" `And` Role "conversation" `Or`ClassName "Skype" `And` Role "ConversationsWindow") n

     -- console coding layout (sucks)
     codingLayout = combineTwoP (Mirror $ TwoPane 0.03 0.66)
                                (layoutHints $ tabbedLayout) (combineTwo (TwoPane 0.03 0.5) tabbedLayout (hintedTile Tall))
                                (ClassName "Gvim" `Or` ClassName "Eclipse")

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
    --, className =? "Plugin-container" --> doFloat
    , className =? "Pidgin"         --> doShift "1:msg"
    , className =? "Gajim"          --> doShift "1:msg"
    , className =? "Skype"          --> doShift "1:msg"
    , className =? "Firefox"        --> doShift "2:web"
    , className =? "Eclipse"        --> doShift "3:dev"
    , className =? "net-minecraft-LauncherFrame" --> doShift "4:media"
    , className =? "Xephyr"         --> doShift "5:media"
    , className =? "VirtualBox"     --> doShift "5:media"
    , className =? "Dolphin-emu"    --> doShift "5:media"
    , className =? "Gimp"           --> doShift "6:misc"
    ]
    <+> scratchpadManageHook (W.RationalRect 0.0 0.0 1.0 0.5)
    <+> manageDocks

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Perform an arbitrary action each time xmonad starts or is restarted with mod-q.
myStartupHook = do
    spawn myTrayerCommand
    return ()

-- the handle event hook to be used with xmonad
myHandleEventHook = ewmhDesktopsEventHook <+> fullscreenEventHook <+> docksEventHook

-- startup for trayer
--myTrayerCommand = "trayer --monitor primary --edge top --align right --SetDockType true --expand true --transparent true --tint 0x000000 --alpha 0 --height 16 --widthtype request"
myTrayerCommand = "trayer --monitor primary --edge top --align right --SetDockType true --expand true --transparent true --tint 0x000000 --alpha 0 --height 16 --widthtype pixel --width 150"

-- command line calls to my dzen2 instances:
-- when using DZen2, use version with Xinerama, XFT and XPM (Option 7 in config.mk)
myDZenXMBar = "dzen2 -xs 1 -h 16 -w 500 -fn 'Inconsolata:size=10' -ta l"
myDZenSTBar = "conky | dzen2 -fn 'Inconsolata:size=10' -xs 1 -ta r -x 500 -h 16 -w 950"

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
                  , ppSort    = fmap (.scratchpadFilterOutWorkspace) $ getSortByXineramaRule
                  }

-- log hook to be used with XMobar (pass a pipe to a running xmobar)
myXmobarLogHook h = dynamicLogWithPP $ xmobarPP {
                      ppOutput = hPutStrLn h
                    , ppTitle = xmobarColor "#FFB6B0" "" . shorten 100
                    , ppCurrent = xmobarColor "#CEFFAC" ""
                    , ppSep = "   "
                    , ppSort = fmap (.scratchpadFilterOutWorkspace) $ ppSort xmobarPP
                    }

myWallpaperHook h = dynamicLogWithPP $ defaultPP {
                       ppOutput  = hPutStrLn h
                     , ppVisible = id
                     , ppCurrent = id
                     , ppSort    = getSortByXineramaRule
                     , ppWsSep   = ","
                     , ppSep     = ""
                     , ppHidden  = (\x -> "")
                     , ppTitle   = (\x -> "")
                     , ppLayout  = (\x -> "")
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

-- which status bar to use - True=Dzen2 False=XMobar
useDZen = True

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
--
mediahost = "mediacenter"
main = do
  rightBar <- if useDZen
                then spawnPipe myDZenSTBar
                else spawnPipe ""
  bar <- if useDZen
            then spawnPipe myDZenXMBar
            else spawnPipe $ "MPD_HOST="++ mediahost ++" ~/.cabal/bin/xmobar ~/.xmonad/xmobar"
  wswp <- spawnPipe "~/.xmonad/wswphook.rb"
  --wswp <- spawnPipe "dzen2"

  replace
  xmonad $ withUrgencyHook NoUrgencyHook $ ewmh $ defaults {
    logHook = do
                setWMName "LG3D"
                ewmhDesktopsLogHook
                -- updatePointer (Relative 0 0)
                if useDZen
                   then myDzenLogHook bar
                   else myXmobarLogHook bar
                myWallpaperHook wswp

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
    , ((myModMask .|. controlMask .|. shiftMask, xK_Right), sendMessage $ Move R)
    , ((myModMask .|. controlMask .|. shiftMask, xK_Left ), sendMessage $ Move L)
    , ((myModMask .|. controlMask .|. shiftMask, xK_Up   ), sendMessage $ Move U)
    , ((myModMask .|. controlMask .|. shiftMask, xK_Down ), sendMessage $ Move D)
    -- override default xmonad restart binding (to kill dzen2)
    , ((myModMask, xK_q), spawn "killall trayer conky dzen2; xmonad --recompile && xmonad --restart")
    -- Screenshot
    , ((0, xK_Print), spawn "scrot -q 95 %Y-%m-%d_%H%M%S.jpg")
    -- Home key
    -- , ((0, 0x1008ff18), spawn "echo not set")
    -- Audio keys
    , ((0, 0x1008ff12), spawn "amixer set Master toggle") -- speaker mute
    , ((0, 0x1008ff11), spawn "amixer set Master 4%-") -- volume up + down
    , ((0, 0x1008ff13), spawn "amixer set Master 4%+")
    , ((0, 0x1008ffb2), spawn "amixer set Mic toggle") -- mic mute
    , ((0, 0x1008ff41), spawn "synclient TouchpadOff=$(synclient -l | grep -c 'TouchpadOff.*=.*0')") --black launch key Thinkvantage
    -- , ((0, keycode 114), spawn "mkdir test")
    -- Fn Media keys which do not work automatically
    , ((0, 0x1008ff2d), spawn "xscreensaver-command -lock")
    , ((0, 0x1008ff8f), spawn "skype")
    , ((0, 0x1008ff59), spawn "screens; nitrogen --restore; killall trayer conky dzen2; xmonad --restart")
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
    , ((myModMask, xK_s), bindOn [
         ("3:dev", do spawn $ myTerminal ++ " -t dev -e 'tmux attach'")
        ,("",         spawn "notify-send \"not specified\"")
        ])
    , ((myModMask, xK_g), goToSelected defaultGSConfig)  -- grid selector
    , ((myModMask, xK_BackSpace), removeWorkspace)
    , ((myModMask, xK_b), sendMessage ToggleStruts)  -- toggle dock spacing
    , ((myModMask .|. shiftMask, xK_o), restart "/home/admin/.xmonad/obtoxmd.sh" True)  -- replace with openbox
    -- scratchpad
    , ((myModMask .|. controlMask, xK_Return),  scratchpadSpawnActionTerminal "urxvt -name scratchpad +sb -e bash -c 'tmux attach -t sp || tmux new -s sp'") --local tmux
    --, ((myModMask .|. controlMask, xK_Return),  scratchpadSpawnActionTerminal "urxvt -name scratchpad +sb -e bash -c 'eval `keychain --eval --nogui -Q -q id_rsa`; TERM=xterm-256color ssh -p 2200 admin@10.130.118.2'")
    -- dynamic workspaces
    , ((myModMask, xK_a      ), selectWorkspace defaultXPConfig)
    , ((myModMask, xK_z      ), renameWorkspace defaultXPConfig)
    ]
    -- better support for dynamic workspaces
    ++
    zip (zip (repeat (myModMask)) [xK_1..xK_9]) (map (withNthWorkspace W.greedyView) [0..])
    ++
    zip (zip (repeat (myModMask .|. shiftMask)) [xK_1..xK_9]) (map (withNthWorkspace W.shift) [0..])
    )

