import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import System.IO

myTerminal = "$TERMINAL"
myModMask = mod4Mask -- Win key or Super_L
myLauncher = "rofi -show-icons -theme solarized_alternate -font 'sans-serif 16' "
myProgramLauncher = myLauncher ++ "-modi combi -show combi -combi-modi drun,run -terminal $TERMINAL"
mySSHLauncher = myLauncher ++ "-show ssh -terminal $TERMINAL"
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

startup :: X ()
startup = do
    spawn "dbus-launch --sh-syntax --exit-with-session &"
    spawn "lxqt-session &"
    spawn "lxqt-policykit-agent &"
    spawn "lxqt-powermanagement &"

myManageHook = composeAll
    [ className =? "lxqt-openssh-askpass" --> doFloat
    , className =? "Xmessage" --> doFloat
    , className =? "firefox" --> doShift "5:browser"
    , className =? "spotify" --> doShift "8:music"
    , className =? "steam" --> doShift "9:steam"
    , manageDocks
    ]

myKeys = 
    [ ((myModMask, xK_p), spawn myProgramLauncher)
    , ((myModMask, xK_v), spawn "pavucontrol")
    , ((myModMask, xK_F5), spawn "env HOME=$XDG_DATA_HOME firefox")
    , ((myModMask, xK_F6), spawn "$TERMINAL -e nvim ~/Documents/Todo.markdown ")
    , ((myModMask, xK_F8), spawn "env HOME=$XDG_DATA_HOME spotify")
    , ((myModMask, xK_F9), spawn "env HOME=$XDG_DATA_HOME steam")
    , ((myModMask, xK_Print), spawn "sleep 0.2; scrot -s")
    , ((0, xK_Print), spawn "sleep 0.2; scrot")
    , ((myModMask .|. shiftMask, xK_x), spawn "lxqt-leave")
    , ((myModMask, 0x1008FF11), spawn "pactl set-sink-volume @DEFAULT_SINK@ -10%")
    , ((myModMask, 0x1008FF13), spawn "pactl set-sink-volume @DEFAULT_SINK@ +10%")
    , ((myModMask, 0x1008FF12), spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
    ]

myWorkspaces = ["1","2","3","4","5:browser","6:work","7","8:music","9:steam","0","-","="]

myPP :: PP
myPP = defaultPP
    { ppCurrent = xmobarColor solViolet ""
    , ppVisible = wrap "" ""
    , ppVisibleNoWindows = Just (\wsId -> xmobarColor solBase1 "" wsId)
    , ppTitle   = xmobarColor solBase01  "" . shorten 40
    , ppWsSep = ", "
    , ppSep = " | "
    , ppUrgent  = xmobarColor solOrange solYellow
    }

myLayout = tiled ||| Mirror tiled ||| noBorders Full
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled   = Tall nmaster delta ratio
    -- The default number of windows in the master pane
    nmaster = 1
    -- Default proportion of screen occupied by master pane
    ratio   = 1/2
    -- Percent of screen to increment by when resizing panes
    delta   = 3/100


main = do
    h <- spawnPipe "xmobar"
    xmonad $ docks defaultConfig
        { manageHook = myManageHook <+> manageHook defaultConfig
        , layoutHook = avoidStruts
            $ spacingRaw True
                         (Border outerGaps outerGaps outerGaps outerGaps) True
                         (Border innerGaps innerGaps innerGaps innerGaps) True
            $ smartBorders myLayout
        , modMask = myModMask
        , startupHook = startup
        , terminal = myTerminal
        , normalBorderColor = solCyan
        , focusedBorderColor = solBlue
        , logHook = dynamicLogWithPP $ myPP { ppOutput = hPutStrLn h }
        -- , keys = myKeys
        , workspaces = myWorkspaces
        , borderWidth = 3
        } `additionalKeys` myKeys
