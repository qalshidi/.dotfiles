import XMonad

import XMonad.Actions.UpdatePointer

import XMonad.Config.Desktop

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns

import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.SpawnOnce

import qualified XMonad.StackSet as W

import Data.Monoid

import System.Exit
import System.IO

myTerminal = "$TERMINAL"
myModMask = mod4Mask -- Win key or Super_L
rofiPrefix = "rofi -theme solarized_alternate -font 'sans-serif 16' "
myLauncher = rofiPrefix ++ "-show-icons"
myProgramLauncher = unwords [myLauncher, "-modi combi -show combi -combi-modi drun,run -terminal $TERMINAL"]
mySSHLauncher = unwords [myLauncher, "-show ssh -terminal $TERMINAL"]
outerGaps = 30
innerGaps = 10

-- solarized color scheme
solBase03 = "#002b36"
solBase02 = "#073642"
solBase01 = "#586e75"
solBase00 = "#657b83"
solBase0 = "#839496"
solBase1 = "#93a1a1"
solBase2 = "#eee8d5"
solBase3 = "#fdf6e3"
solYellow = "#b58900"
solOrange = "#cb4b16"
solRed = "#dc322f"
solMagenta = "#d33682"
solViolet = "#6c71c4"
solBlue = "#268bd2"
solCyan = "#2aa198"
solGreen = "#859900"

myStartupHook :: X ()
myStartupHook = do
    spawnOnce "lxqt-session &"
    spawnOnce "lxqt-policykit-agent &"
    spawnOnce "udiskie --tray &>> /tmp/udiskie.log &"
    spawnOnce "nitrogen --restore &"

myManageHook :: Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
    [ className =? "lxqt-openssh-askpass" --> doFloat
    , className =? "Xmessage"             --> doFloat
    , className =? "mpv"                  --> doFloat
    , className =? "thunderbird"          --> doShift "4:email"
    , className =? "firefox"              --> doShift "5:browser"
    , className =? "spotify"              --> doShift "8:music"
    , className =? "Steam"                --> doShift "9:steam"
    , isFullscreen                        --> doFullFloat
    , manageDocks
    ]

myKeys = 
    [ ((myModMask                , xK_p        ), spawn myProgramLauncher)
    , ((myModMask .|. shiftMask  , xK_p        ), spawn "rofipass")
    , ((myModMask .|. shiftMask  , xK_m        ), spawn $ rofiPrefix ++ "-show emoji -modi emoji")
    , ((myModMask                , xK_s        ), spawn mySSHLauncher)
    , ((myModMask                , xK_backslash), spawn myTerminal)
    , ((myModMask                , xK_Tab      ), sendMessage NextLayout)
    , ((myModMask .|. controlMask, xK_j        ), windows W.swapDown)
    , ((myModMask .|. controlMask, xK_k        ), windows W.swapUp)
    , ((myModMask .|. controlMask, xK_backslash), spawn $ myTerminal ++ " -e ranger")
    , ((myModMask                , xK_BackSpace), kill)
    , ((myModMask                , xK_v        ), spawn "pavucontrol")
    , ((myModMask                , xK_F4       ), spawn "thunderbird")
    , ((myModMask                , xK_F5       ), spawn "firefox")
    , ((myModMask                , xK_F6       ), spawn "$TERMINAL -e nvim ~/Documents/Todo/*todo.txt")
    , ((myModMask                , xK_F8       ), spawn "env HOME=$XDG_DATA_HOME spotify")
    , ((myModMask                , xK_F9       ), spawn "env steam")
    , ((myModMask                , xK_Print    ), spawn "sleep 0.2; scrot -s ~/Pictures/Screenshots/screenshot.png")
    , ((0                        , xK_Print    ),  spawn "sleep 0.2; scrot ~/Pictures/Screenshots/screenshot.png")
    , ((myModMask                , xK_Escape   ), spawn "lxqt-leave")
    , ((0                        , 0x1008FF11  ), spawn "pactl set-sink-volume @DEFAULT_SINK@ -10%")
    , ((0                        , 0x1008FF13  ), spawn "pactl set-sink-volume @DEFAULT_SINK@ +10%")
    , ((0                        , 0x1008FF12  ), spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
    , ((0                        , 0x1008FF02  ), spawn "lxqt-backlight_backend --inc")
    , ((0                        , 0x1008FF03  ), spawn "lxqt-backlight_backend --dec")
    -- Quit xmonad
    , ((myModMask .|. shiftMask  , xK_Escape   ), io (exitWith ExitSuccess))
    -- Restart xmonad
    , ((myModMask                , xK_r        ), spawn "xmonad --recompile; xmonad --restart")
    ]

    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    ++
    [ ((m .|. myModMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_q] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
    ]

myWorkspaces =
    [ "1:terminal"
    , "2:terminal"
    , "3:chat"
    , "4:email"
    , "5:browser"
    , "6:other"
    , "7:other"
    , "8:music"
    , "9:steam"
    ]

-- myPP :: PP
-- myPP = def
--     { ppCurrent = currentLeftWrap . xmobarColor solOrange solBase03 . pad
--     , ppVisible = xmobarColor solBase02 solYellow . visibleLeftWrap . pad
--     , ppVisibleNoWindows = Just (\wsId -> xmobarColor solBase1 "" wsId)
--     , ppTitle   = xmobarColor solBase01  "" . shorten 40
--     , ppWsSep = ", "
--     , ppSep = " | "
--     , ppUrgent  = xmobarColor solRed solOrange
--     }
--     where
--       currentLeftWrap = wrap
--                           (xmobarColor solBase02 "" "\57556")
--                           (xmobarColor solBase02 "" "\57554")
--       visibleLeftWrap = wrap
--                           (xmobarColor solYellow "" "\57556")
--                           (xmobarColor solYellow "" "\57554")

-- Layout
-- ======
myLayout =  threecol ||| tiled ||| (Mirror tiled) ||| noBorders Full
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled   = Tall nmaster delta ratio
    -- The default number of windows in the master pane
    nmaster = 1
    -- Default proportion of screen occupied by master pane
    ratio   = 1/2
    -- Percent of screen to increment by when resizing panes
    delta   = 3/100
    threecol = ThreeColMid nmaster delta ratio

myLayoutHook
    = smartBorders
    . spacingRaw
        True
        (Border outerGaps outerGaps outerGaps outerGaps) True
        (Border innerGaps innerGaps innerGaps innerGaps) True
    $   myLayout

-- Main
-- ====
main :: IO()
main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ desktopConfig
        { manageHook         = myManageHook <+> manageHook desktopConfig
        , handleEventHook    = fullscreenEventHook <+> handleEventHook desktopConfig
        , layoutHook         = desktopLayoutModifiers $ myLayoutHook
        , modMask            = myModMask
        , startupHook        = myStartupHook <+> startupHook desktopConfig
        , terminal           = myTerminal
        , normalBorderColor  = solCyan
        , focusedBorderColor = solOrange
        , logHook            = do
                                   -- dynamicLogWithPP $ myPP { ppOutput = hPutStrLn xmproc }
                                   updatePointer (0.5, 0.5) (0, 0)
                                   logHook desktopConfig
        , workspaces         = myWorkspaces
        , borderWidth        = 3
        } `additionalKeys` myKeys
