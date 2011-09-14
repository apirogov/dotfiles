-- xmonad config used by Anton Pirogov

import System.IO
import System.Exit
import XMonad hiding (Tall)

import XMonad.Actions.PhysicalScreens
import XMonad.Actions.CycleWS

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.RestoreMinimized

import XMonad.Layout.ComboP
import XMonad.Layout.Reflect
import XMonad.Layout.TwoPane
import XMonad.Layout.HintedTile
import XMonad.Layout.Tabbed
import XMonad.Layout.Grid
import XMonad.Layout.Spiral
import XMonad.Layout.Circle

import XMonad.Layout.Named
import XMonad.Layout.Minimize
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.WindowNavigation

import XMonad.Util.Run (spawnPipe)
import XMonad.Util.Scratchpad
import XMonad.Util.WorkspaceCompare

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "lxterminal"

-- Width of the window border in pixels.
--
myBorderWidth   = 1

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
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
--
myNumlockMask   = mod2Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--

myWorkspaces    = ["1:msg","2:web","3:code","4:media","5:misc"]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#7c7c7c"
myFocusedBorderColor = "#ffb6b0"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $

    -- launch a terminal
    [ ((modMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- launch dmenu
    , ((modMask,               xK_p     ), spawn "exe=`dmenu_path | ~/bin/dmenu` && eval \"exec $exe\"")

    -- launch gmrun
    , ((modMask .|. shiftMask, xK_p     ), spawn "gmrun")

    -- close focused window
    , ((modMask .|. shiftMask, xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modMask,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modMask,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modMask,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modMask,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modMask,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modMask,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modMask,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modMask,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modMask,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))

    -- toggle the status bar gap
    -- TODO, update this binding with avoidStruts , ((modMask              , xK_b     ),

    -- Quit xmonad
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modMask              , xK_q     ), restart "xmonad" True)


    -- Minimize window
    , ((modMask .|. shiftMask, xK_m     ), withFocused (\f -> sendMessage (MinimizeWin f)))

    -- improved WindowNavigation keybindings
    , ((modMask,               xK_Right), sendMessage $ Go R)
    , ((modMask,               xK_Left ), sendMessage $ Go L)
    , ((modMask,               xK_Up   ), sendMessage $ Go U)
    , ((modMask,               xK_Down ), sendMessage $ Go D)
    , ((modMask .|. shiftMask, xK_Right), sendMessage $ Swap R)
    , ((modMask .|. shiftMask, xK_Left ), sendMessage $ Swap L)
    , ((modMask .|. shiftMask, xK_Up   ), sendMessage $ Swap U)
    , ((modMask .|. shiftMask, xK_Down ), sendMessage $ Swap D)
    , ((modMask .|. shiftMask, xK_s    ), sendMessage $ SwapWindow)  --Swap between panes in ComboP TwoPane

    -- Screenshot
    , ((0, xK_Print), spawn "scrot -q 95 %Y-%m-%d_%H%M%S.jpg")
    -- MPD multimedia keys
    , ((0, 0x1008ff13), spawn "mpc volume +10")
    , ((0, 0x1008ff11), spawn "mpc volume -10")
    , ((modMask .|. controlMask, xK_p), spawn "mpc toggle")
    , ((modMask .|. controlMask, xK_s), spawn "mpc random")
    , ((modMask .|. controlMask, xK_r), spawn "mpc repeat")
    , ((modMask .|. controlMask, xK_f), spawn "mpc next")
    , ((modMask .|. controlMask, xK_b), spawn "mpc prev")

    -- Scratchpad terminal
    , ((modMask .|. controlMask, xK_Return),  scratchpadSpawnAction defaultConfig)

    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modMask, key), f sc)
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(viewScreen, 0), (sendToScreen, shiftMask)]]

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.



myLayout = windowNavigation $ avoidStruts $ nameTail $ minimize
           ( defaultPerWorkspace ||| hintedTile Tall ||| hintedTile Wide ||| tabbedLayout ||| Full ||| spiral (6/7) ||| Circle ||| Grid )
  where
     defaultPerWorkspace = named "Workspace Default"
                         $ onWorkspace "1:msg"   pidginLayout
                         $ onWorkspace "2:web"   tabbedLayout
                         $ onWorkspace "3:code"  codingLayout
                         $ onWorkspace "4:media" Full
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
     myTabConfig = defaultTheme {   activeBorderColor = "#7C7C7C"
                             , activeTextColor = "#CEFFAC"
                             , activeColor = "#000000"
                             , inactiveBorderColor = "#7C7C7C"
                             , inactiveTextColor = "#EEEEEE"
                             , inactiveColor = "#000000" }

     -- combined workspace layout for usage with instant messenger (keeping the Pidgin buddy list always on the right)
     pidginLayout = reflectHoriz $ combineTwoP (TwoPane (3/100) (1/5))
                                               (hintedTile Tall) (hintedTile Wide)
                                               (ClassName "Pidgin" `And` Role "buddy_list")

     -- default coding layout
     codingLayout = combineTwoP (Mirror $ TwoPane (3/100) (2/3))
                                (tabbedLayout) (hintedTile Tall)
                                (ClassName "Gvim")

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll [
      className =? "Gimp"           --> doFloat
    , className =? "Vlc"            --> doFloat
    , className =? "Pidgin"         --> doShift "1:msg"
    , className =? "Firefox"        --> doShift "2:web"
    , className =? "net-minecraft-MinecraftLauncher" --> doShift "4:media"
    , className =? "VirtualBox"     --> doShift "4:media"
    ]

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True


------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'DynamicLog' extension for examples.
--
-- To emulate dwm's status bar
--
-- > logHook = dynamicLogDzen
--

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = do
    --temporary, needed as long as the monitors are swapped
    viewScreen 0
    swapNextScreen
    ---
--    spawn "firefox"
    return ()

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
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
    , startupHook        = myStartupHook
	}

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
        modMask            = myModMask,
        numlockMask        = myNumlockMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = smartBorders $ myLayout,
        manageHook         = myManageHook,
        startupHook        = myStartupHook
    }
