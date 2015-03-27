{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -W -fwarn-unused-imports -fno-warn-missing-signatures #-}
-- xmonad config of Anton Pirogov
-- requires trayer-srg-git + dzen2 + conky + xmonad-contrib-darcs
---------------------------------------------------------------
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Control.Applicative ((<$>))
import Control.Monad (when)

import XMonad
import qualified XMonad.StackSet as W
import System.Exit (exitSuccess)

import XMonad.Actions.CycleWS (nextScreen,prevScreen,shiftNextScreen,shiftPrevScreen)
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.Navigation2D
import XMonad.Actions.RotSlaves (rotAllUp,rotAllDown)
import XMonad.Actions.UpdatePointer (updatePointer)

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ServerMode
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.WallpaperSetter

import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Replace
import XMonad.Util.Run (runProcessWithInput, runInTerm)
import XMonad.Util.WorkspaceCompare (getSortByXineramaRule,getSortByIndex)
import qualified XMonad.Util.ExtensibleState as XS

import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.Shell

-- base layouts
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Tabbed
-- layout modifiers
import XMonad.Layout.BorderResize
import XMonad.Layout.NoBorders (smartBorders,noBorders)
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.Layout.TrackFloating
import XMonad.Layout.WindowSwitcherDecoration
-- layout combinators
import XMonad.Layout.IM
import XMonad.Layout.MultiToggle
import XMonad.Layout.Reflect (REFLECTX(..),REFLECTY(..))

---------------------------------------------------------------
-- The preferred terminal program, which is used in a binding below and by certain contrib modules.
myTerminal           = "urxvt"
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
  where myTabConfig  = def {  activeBorderColor = "#7C7C7C" , inactiveBorderColor = "#7C7C7C"
                            , activeTextColor   = "#CEFFAC" , inactiveTextColor   = "#EEEEEE"
                            , activeColor       = "#000000" , inactiveColor       = "#000000"
                            }

-- my main layout with the modifier and toggle stack
myLayout = trackFloating $ smartBorders $ avoidStruts $ mkToggle1 NBFULL $ mkToggle1 TABBED
         $ renamed [Replace "Bsp"] {- $ winSwitcher -} $ borderResize $ spacingWithEdge 0
         $ mkToggle1 REFLECTX $ mkToggle1 REFLECTY $ imLayout $ emptyBSP
  where winSwitcher l = renamed [CutWordsLeft 1]
          $ windowSwitcherDecoration shrinkText def{activeColor="#002590"} l
        imLayout    = (renamed [CutWordsLeft 1]) . (withIM (1/8)
                      (ClassName "Pidgin" `And` Role "buddy_list"
                      `Or` ClassName "Gajim" `And` Role "roster"
                      `Or` ClassName "Skype" `And`
                        (Not $ (Role "ConversationsWindow" `Or` Role "CallWindow"))))

------------------------------------------------------------------------
-- Execute arbitrary actions and WindowSet manipulations when managing a new window.
-- (use the xprop utility to get window attributes). possible attributes:
-- resource/appName: first elem. of WM_CLASS
-- className: second elem. title: WM_NAME
-- others: e.g. stringProperty "WM_WINDOW_ROLE" to access it.
myManageHook = namedScratchpadManageHook scratchpads <+> manageDocks

-- Perform an arbitrary action each time xmonad starts or is restarted with mod-q.
myStartupHook = spawn myTrayerCommand <+> spawn myDzenConky <+> updateConkyMPD

myHandleEventHook = ewmhDesktopsEventHook <+> fullscreenEventHook <+> docksEventHook
                <+> serverModeEventHookF "GOTO_WS" goToWS
                <+> serverModeEventHookF "TOG_FUL" (const $ sendMessage $ Toggle NBFULL)
                <+> serverModeEventHookF "TOG_TAB" (const $ sendMessage $ Toggle TABBED)

myLogHook = setWMName "LG3D" <+> ewmhDesktopsLogHook
        <+> updatePointer (0.5, 0.5) (0,0) <+> wallpaperSetter def

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
        handleEventHook    = myHandleEventHook,
        logHook            = myLogHook
    }

-- command line calls to my dzen2 and trayer instances
-- use DZen2 version with Xinerama, XFT and XPM (Option 7 in config.mk)
myDzenStyle = " -e 'onstart=lower' -fn 'Inconsolata:size=10' -xs 1 -h 16"
myDzenStatus = "dzen2 -ta l -w 500" ++ myDzenStyle
myDzenConky  = "conky -c ~/.xmonad/conkyrc | dzen2 -ta r -x 500 -w 1000" ++ myDzenStyle
myTrayerCommand = "trayer -l --monitor primary --edge top --align right --expand true "
                ++"--transparent true --tint 0x000000 --alpha 0 --height 16 --widthtype pixel --width 100"

-- log hook for usage with dzen2
myDzenPP = def { ppCurrent = dzenColor "#ffffff" "#404040" . pad
               , ppVisible = dzenColor "#ffffff" "" . pad . clickWS
               , ppHidden  = dzenColor "#808080" "" . pad . clickWS
               , ppUrgent  = dzenColor "#ffff00" "" . pad . clickWS . dzenStrip
               , ppHiddenNoWindows = const ""
               , ppWsSep   = ""
               , ppSep     = " "
               , ppTitle   = dzenColor "#ffffff" ""
               , ppLayout  = (\x -> click 3 (xctl "TOG_TAB" "") $ click 1 (xctl "TOG_FUL" "")
                                  $ dzenColor (getWSColor x) "" x)
               , ppSort    = (.namedScratchpadFilterOutWorkspace) <$> getSortByXineramaRule
               }
  where click btn cmd str = "^ca("++(show btn)++","++cmd++")"++str++"^ca()"
        clickWS name = click 1 (xctl "GOTO_WS" name) name
        rainbow = ["#b0b0b0","#b000b0","#4040ff","#00d0d0","#00f000","#f0f000","#ffa000","#ff0000"]
        getWSColor x = (rainbow !!) $ (1+) $ fromMaybe (-1) (x `elemIndex` ["Bsp","Ful","Tab"])
        xctl ev param = "~/.xmonad/xmonadctl -a "++ev++" \""++param++"\""

----

-- dmenu-like colors
myXPConfig = def {
    position = Top , promptBorderWidth = 0
  , bgColor = "#202020",  fgColor = "#d0d0d0"
  , bgHLight = "#004080", fgHLight = "#f0f0f0"
  }

-- selection prompt that shows all options from the start
select msg sel = inputPromptWithCompl myXPConfig msg compl
  where compl "" = return sel
        compl s  = mkComplFunFromList sel s
-- perform an action after selecting an item
selectThenDo msg sel f = select msg sel >>= flip whenJust f

-- confirm action before performing
confirm :: String -> X () -> X ()
confirm msg f = selectThenDo msg ["n","y"] (\s -> when (s=="y") f)

-- my universal prompt - overloaded shell prompt with nested sub-prompts
myPrompt = do
  cmds <- io getCommands
  ret <- inputPromptWithCompl myXPConfig "" $ myCompl (cmds++map fst prompthooks)
  whenJust ret $ exec
  where myCompl = getShellCompl
        exec s
          | Just h <- lookup p prompthooks = h
          | s `elem` tp = runInTerm "" s
          | otherwise   = spawn s
          where (p,_) = break (==':') s
        tp = ["ranger","ncmpcpp","ssh","vim"] -- programs to run in terminal
        prompthooks = [("n",netctlP)          -- commands calling sub-prompts
                      ,("c",calcP)]

-- prompt to select netctl config to connect to (first disconnects all)
netctlP = do
  nets <- lines <$> runProcessWithInput "bash"
                   ["-c", "find /etc/netctl/ -maxdepth 1 -type f | sed 's/^.*\\///'"] ""
  selectThenDo "netctl" ("-":nets) (spawn . ("sudo netctl stop-all && sudo netctl start "++))

-- use ghc as calculator
calcP = do
  ret <- inputPrompt myXPConfig "calc"
  whenJust ret $ \n -> spawn $ "bash -c \"notify-send $(ghc -e '"++n++"')\""

-- type to store chosen MPD server in X State
newtype MPDHost = MPDHost { mpdHost :: String } deriving (Read,Show,Typeable)
instance ExtensionClass MPDHost where
    initialValue = MPDHost "localhost"
    extensionType = PersistentExtension

-- select the MPD to be controlled by keybindings
mpdP = do
  mpds <- ("localhost":).lines <$> runProcessWithInput "cat" [".mpdpwd"] ""
  let hosts = map (\h -> let (l,r) = break (=='@') h in if null r then l else tail r) mpds
  selectThenDo "MPD" hosts (\h ->
    XS.put (MPDHost $ fromMaybe (head mpds) $ lookup h $ zip hosts mpds) >> updateConkyMPD)

updateConkyMPD = XS.gets mpdHost >>= \h -> spawn $ "echo '" ++ h ++ "' > /tmp/currmpd"
----

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

-- switch workspace, remove if it is empty and not in default list (dynamicWS)
goToWS ws = removeEmptyWorkspaceAfterExcept myWorkspaces (return ()) >> addWorkspace ws

----

scratchpads = [
  NS "scratchpad" "urxvt -name scratchpad +sb -e bash -c 'tmux attach -t scratch || tmux new -s scratch'"
     (resource =? "scratchpad") (customFloating $ W.RationalRect 0 0 1.0 0.5),
  NS "neoview" "feh --title neoview ~/.xmonad/neo-druckvorlage.png"
     (title =? "neoview") (customFloating $ W.RationalRect 0 0 1.0 0.33)
  ]
scratch = namedScratchpadAction scratchpads

------------------------------------------------------------------------
main = do
  replace
  xmonad =<< (statusBar myDzenStatus myDzenPP (const (myModMask, xK_y))
    $ withNavigation2DConfig def { defaultTiledNavigation = centerNavigation }
    $ withUrgencyHook NoUrgencyHook $ ewmh $ myConf
    `additionalKeys` ([  -- Key bindings --
      ((myModMask, xK_q), spawn xmRestart)
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
    , ((myModMask,                xK_x), sendMessage $ Toggle NBFULL)
    , ((myModMask .|. shiftMask,  xK_x), sendMessage $ Toggle TABBED)
    , ((myModMask,                xK_v), sendMessage $ Toggle REFLECTY)
    , ((myModMask .|. shiftMask,  xK_v), sendMessage $ Toggle REFLECTX)

    -- rotate windows (BSPWM-like circulate)
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

    -- scratchpad
    , ((myModMask, xK_BackSpace), scratch "scratchpad")
    , ((myModMask, xK_Delete), windows W.focusDown <+> scratch "neoview" <+> windows W.focusUp)

    -- media keys which do not work automatically
    , ((0, xK_Print), spawn "scrot -q 95 %Y-%m-%d_%H%M%S.jpg") -- Screenshot
    , ((0, xK_Scroll_Lock), spawn togKBLayout)              -- ScrLck on Thinkpad L530 = Fn+K
    , ((0, 0x1008ff41), spawn togTouchpad)                  -- black launch key Thinkvantage
    , ((0, 0x1008ff02), spawn "xbacklight -inc 10")
    , ((0, 0x1008ff03), spawn "xbacklight -dec 10")
    , ((0, 0x1008ff2d), spawn "xscreensaver-command -lock") -- lock key on thinkpad
    , ((0, 0xff25),     spawn "xscreensaver-command -lock") -- Fn-Esc on my TECK
    , ((0, 0x1008ff59), spawn "screens" >> spawn xmRestart) -- key with projector
    -- , ((0, 0x1008ff2f), spawn "sudo systemctl suspend")     -- suspend media key (systemd takes care (logind.conf))

    -- system volume keys
    , ((0, 0x1008ff12), spawn "amixer set Master toggle")  -- speaker mute
    , ((0, 0x1008ff11), spawn "amixer set Master 5%-")     -- volume up + down
    , ((0, 0x1008ff13), spawn "amixer set Master 5%+")
    , ((0, 0x1008ffb2), spawn "amixer set Capture toggle") -- mic mute

    -- For music control
    , ((myModMask, xK_m), mpdP)
    , ((myModMask .|. shiftMask, xK_m), runMpdClient)
    , ((0, 0x1008ff14), mpccmd "toggle")
    , ((0, 0x1008ff17), mpccmd "next")
    , ((0, 0x1008ff16), mpccmd "prev")
    , ((myModMask, 0x1008ff11), mpccmd "volume -10")
    , ((myModMask, 0x1008ff13), mpccmd "volume +10")
    ]
    ++ -- dynamic workspaces for all unused numbers (will be auto-added + auto-removed when empty)
    (drop (length myWorkspaces) $ zip (zip (repeat myModMask) $ [xK_1..xK_9]++[xK_0])
        (map goToWS $ map show $ [1..9]++[0]))
    ++
    (drop (length myWorkspaces) $ zip (zip (repeat (myModMask .|. shiftMask)) $ [xK_1..xK_9]++[xK_0])
        (map (withNamedWorkspace W.shift) $ map show $ [1..9]++[0]))
    ))
    where xmRestart = "xmonad --recompile && killall trayer conky dzen2; xmonad --restart"
          mpccmd str = XS.gets mpdHost >>= \h -> spawn $ "mpc -h "++h++" "++str
          runMpdClient = XS.gets mpdHost >>= \h -> runInTerm "" $ "ncmpcpp -h "++h
          togTouchpad = "synclient TouchpadOff=$(synclient -l | grep -c 'TouchpadOff.*=.*0')"
          togKBLayout = "setxkbmap -v | grep 'us('; if [[ \"$?\" == '0' ]]; then setxkbmap de neo -option;"
                       ++ " else setxkbmap us cz_sk_de -option -option caps:escape; fi"
