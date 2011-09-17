-- xmonad config used by Anton Pirogov
-- Copyright © 2011 Anton Pirogov
---------------------------------------------------------------

import System.IO
import System.Exit
import XMonad hiding (Tall)

-- actions
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.PerWorkspaceKeys

-- hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.RestoreMinimized

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
import XMonad.Layout.Minimize
import XMonad.Layout.NoBorders
import XMonad.Layout.Reflect

-- layout combinators
import XMonad.Layout.IM
import XMonad.Layout.ComboP
import XMonad.Layout.TwoPane

-- other stuff
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeys)

import XMonad.Util.Scratchpad
import XMonad.Util.WorkspaceCompare

-- essentials
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

---------------------------------------------------------------

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
myTerminal      = "lxterminal"

-- Width of the window border in pixels.
myBorderWidth   = 1

-- Border colors for unfocused and focused windows, respectively.
myNormalBorderColor  = "#7c7c7c"
myFocusedBorderColor = "#ffb6b0"

-- Modifier key used for keybindings. Default is usually mod4Mask (windows key).
myModMask       = mod4Mask

-- The mask for the numlock key. Numlock status is "masked" from the
-- current modifier status, so the keybindings will work with numlock on or
-- off. You may need to change this on some systems.
--
-- You can find the numlock modifier by running "xmodmap" and looking for a
-- modifier with Num_Lock bound to it:
--
-- > $ xmodmap | grep Num
-- > mod2        Num_Lock (0x4d)
--
-- Set numlockMask = 0 if you don't have a numlock key, or want to treat
-- numlock status separately.
myNumlockMask   = mod2Mask

-- The default number of workspaces (virtual screens) and their names.
-- The number of workspaces is determined by the length of this list.
myWorkspaces    = ["1:msg","2:web","3:code","4:media","5:misc"]

------------------------------------------------------------------------
-- Layouts:
-- The available layouts. Each alternative layout is separated by |||,
--
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state.

myLayout = windowNavigation $ avoidStruts $ nameTail $ minimize
           ( defaultPerWorkspace ||| hintedTile Tall ||| hintedTile Wide ||| tabbedLayout ||| Full ||| spiral (6/7) ||| Circle ||| Grid )
  where
     defaultPerWorkspace = named "Workspace Default"
                         $ onWorkspace "1:msg"   pidginLayout
                         $ onWorkspace "2:web"   tabbedLayout
                         $ onWorkspace "3:code"  codingLayout
                         $ onWorkspace "4:media" Full
                         $ onWorkspace "5:misc"  gimp
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

     -- combined workspace layout for usage with instant messenger (keeping the Pidgin buddy list always on the right)
     pidginLayout = reflectHoriz $ withIM (1/5) (ClassName "Pidgin" `And` Role "buddy_list") (hintedTile Wide)

     -- default coding layout
     codingLayout = combineTwoP (Mirror $ TwoPane (3/100) (2/3))
                                (tabbedLayout) (hintedTile Tall)
                                (ClassName "Gvim")

     gimp = withIM (0.11) (Role "gimp-toolbox") $
             reflectHoriz $
             withIM (0.15) (Role "gimp-dock") Full

------------------------------------------------------------------------
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace. (use the xprop utility to get window attributes)
myManageHook = composeAll [
      className =? "Vlc"            --> doFloat
    , className =? "Pidgin"         --> doShift "1:msg"
    , className =? "Firefox"        --> doShift "2:web"
    , className =? "VirtualBox"     --> doShift "4:media"
    , className =? "Gimp"           --> doShift "5:misc"
    , className =? "net-minecraft-MinecraftLauncher" --> doShift "4:media"
    ]

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Perform an arbitrary action each time xmonad starts or is restarted with mod-q.
myStartupHook = do
--    spawn "firefox"
    return ()

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
        numlockMask        = myNumlockMask,
        workspaces         = myWorkspaces,

      -- hooks, layouts
        layoutHook         = smartBorders $ myLayout,
        manageHook         = myManageHook,
        startupHook        = myStartupHook
    }

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
--
main = do
  xmproc <- spawnPipe "~/.cabal/bin/xmobar ~/.xmonad/xmobar"
  xmonad $ ewmh $ defaults {
      logHook            = do
                            setWMName "LG3D"
                            ewmhDesktopsLogHook
                            dynamicLogWithPP $ xmobarPP {
                                ppOutput = hPutStrLn xmproc
                                , ppTitle = xmobarColor "#FFB6B0" "" . shorten 100
                                , ppCurrent = xmobarColor "#CEFFAC" ""
                                , ppSort = fmap (.scratchpadFilterOutWorkspace) getSortByIndex
                                , ppSep = "   "
                            }
    , manageHook         = scratchpadManageHook (W.RationalRect 0 0 1 (1/2))
                          <+> manageDocks
                          <+> myManageHook
    , handleEventHook    = do ewmhDesktopsEventHook
                              restoreMinimizedEventHook

	} `additionalKeys` ( [  -- Key bindings --
    -- Minimize window
      ((myModMask .|. shiftMask, xK_m     ), withFocused (\f -> sendMessage (MinimizeWin f)))
    -- improved WindowNavigation keybindings
    , ((myModMask,               xK_Right), sendMessage $ Go R)
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
    -- MPD multimedia keys
    , ((0, 0x1008ff13), spawn "mpc volume +10")
    , ((0, 0x1008ff11), spawn "mpc volume -10")
    , ((myModMask .|. controlMask, xK_p), spawn "mpc toggle")
    , ((myModMask .|. controlMask, xK_s), spawn "mpc random")
    , ((myModMask .|. controlMask, xK_r), spawn "mpc repeat")
    , ((myModMask .|. controlMask, xK_f), spawn "mpc next")
    , ((myModMask .|. controlMask, xK_b), spawn "mpc prev")
    -- Scratchpad terminal
    , ((myModMask .|. controlMask, xK_Return), scratchpadSpawnActionTerminal "urxvt")
    -- per workspace keys
    , ((myModMask .|. controlMask, xK_r), bindOn [
         ("3:code", spawn "pcmanfm" >> spawn myTerminal >> spawn "gvim")
        ,("",       spawn "notify-send \"not specified\"")
        ])
    ]
    ++  -- Override monitor focus keys with PhysicalScreens functions
    [((m .|. myModMask, key), f sc)
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(viewScreen, 0), (sendToScreen, shiftMask)]]
    )

