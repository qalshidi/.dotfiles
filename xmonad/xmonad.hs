import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

myTerminal    = "$TERMINAL"
myModMask     = mod4Mask -- Win key or Super_L
myBorderWidth = 3
myLauncher = "rofi -show-icons -theme solarized_alternate -font 'sans-serif 16' "
myProgramLauncher = myLauncher ++ "-modi combi -show combi -combi-modi drun,run -terminal $TERMINAL"
mySSHLauncher = myLauncher ++ "-show ssh -terminal $TERMINAL"


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
    , ((myModMask .|. shiftMask, xK_e), spawn "lxqt-leave")
    , ((myModMask, 0x1008FF11), spawn "pactl set-sink-volume @DEFAULT_SINK@ -10%")
    , ((myModMask, 0x1008FF13), spawn "pactl set-sink-volume @DEFAULT_SINK@ +10%")
    , ((myModMask, 0x1008FF12), spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
    ]

main = do
    h <- spawnPipe "xmobar"
    xmonad $ myConfig

myConfig = defaultConfig
    { manageHook = myManageHook <+> manageHook defaultConfig
    , layoutHook = avoidStruts  $  layoutHook defaultConfig
    , modMask = myModMask
    , startupHook = startup
    , terminal = myTerminal
    , logHook = dynamicLogWithPP $ xmobarPP { ppOutput = hPutStrLn h }
    -- , keys = myKeys
    , workspaces = ["1","2","3","4","5:browser","6:work","7","8:music","9:steam","0","-","="]
    } `additionalKeys` myKeys

