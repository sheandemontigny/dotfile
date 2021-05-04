import           Control.Monad
import           Data.List
import           System.Exit
import           XMonad
import           XMonad.Actions.CycleWS
import           XMonad.Config.Desktop
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Layout.Gaps
import           XMonad.Layout.IndependentScreens
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Spacing
import           XMonad.Prompt.ConfirmPrompt
import qualified XMonad.StackSet               as W
import           XMonad.Util.Dmenu
import           XMonad.Util.EZConfig
import           XMonad.Util.Run
import           XMonad.Util.SpawnOnce

-- Constant
myTerminal = "alacritty"

myBorderWidth = 2

-- myNormalBorderColor = "#3b4252"
myNormalBorderColor = "#071e3d"

-- myFocusesBorderColor = "#bc96da"
myFocusesBorderColor = "#278ea5"

myModMask = mod4Mask

myWorkspaces =
  ["dev_1", "dev_2", "web", "proc", "api", "vbox", "chat", "music", "softdb"]

-- Application to start on boot
myStartupHook = do
  spawnOnce "nitrogen --restore &"
  spawnOnce "dunst &"
  spawnOnce "discord &"
  spawnOnce "teams &"
  spawnOnce "stalonetray &"
  spawnOnce "nm-applet &"

-- Layout
windowGaps i = spacingRaw False (Border i 0 i 0) True (Border 0 i 0 i) True

screenGaps = gaps [(L, 0), (R, 0), (U, 40), (D, 0)]

myDefaultLayout = Tall 1 (3 / 100) (1 / 2) ||| Full

myLayoutHook = windowGaps 16 $ screenGaps $ myDefaultLayout

dmenuArg = ["-i", "-x", "16", "-y", "8", "-z", "3624", "-h", "34"]


confirm :: String -> X () -> X ()
confirm m f = do
  result <- menuArgs "dmenu" dmenuArg [m]
  when (result == m) f

-- Keybinding
myKeys =
  [ ("M-S-q", confirm "quit" $ io (exitWith ExitSuccess))
    , ("M-<F1>"                , spawn "multilockscreen -l")
    , ("M-p", spawn $ "dmenu_run -p 'Run: ' " ++ intercalate " " dmenuArg)
    , ("M-<Tab>"               , sendMessage NextLayout)
    , ("M-,"                   , nextScreen)
    , ("M-."                   , prevScreen)
    , ("<XF86AudioLowerVolume>", spawn "pamixer -d 2")
    , ("<XF86AudioRaiseVolume>", spawn "pamixer -i 2")
    , ("<XF86AudioMute>"       , spawn "pamixer -t")
    , ("<XF86AudioPlay>"       , spawn "playerctl play-pause")
    , ("<XF86AudioPrev>"       , spawn "playerctl previous")
    , ("<XF86AudioNext>"       , spawn "playerctl next")
    ]
    -- Prevent workspace swap if toggle between two visible workspace
    ++ [ (otherModMasks ++ "M-" ++ [key], action tag)
       | (tag          , key   ) <- zip myWorkspaces "123456789"
       , (otherModMasks, action) <-
         [("", windows . W.view), ("S-", windows . W.shift)]
       ]

-- Rule for application
myManageHook = composeAll
  [ appName =? "crx_baipgmmeifmofkcilhccccoipmjccehn" --> doFloat
  , className =? "Gimp" --> doFloat
  , className =? "Microsoft Teams - Preview" --> doShift (myWorkspaces !! 8)
  , className =? "discord" --> doShift (myWorkspaces !! 6)
  , isDialog --> doCenterFloat
  , stringProperty "WM_WINDOW_ROLE" =? "pop-up" --> doFloat
  ]

-- Main application. Config xmonad and start xmobar.
main :: IO ()
main = do
  xmproc0 <- spawnPipe "xmobar -x 0 $HOME/.config/xmobar/xmobarrc0"
  xmproc1 <- spawnPipe "xmobar -x 1 $HOME/.config/xmobar/xmobarrc1"

  xmonad
    $                 ewmh
    $                 docks def
                        { terminal           = myTerminal
                        , modMask            = myModMask
                        , borderWidth        = myBorderWidth
                        , normalBorderColor  = myNormalBorderColor
                        , focusedBorderColor = myFocusesBorderColor
                        , workspaces         = myWorkspaces
                        , startupHook        = myStartupHook
                        , layoutHook         = myLayoutHook
                        , manageHook         = myManageHook
                        , logHook            = dynamicLogWithPP xmobarPP
                          { ppOutput = \x -> hPutStrLn xmproc0 x >> hPutStrLn xmproc1 x
                          , ppCurrent = xmobarColor "#98be65" "" . wrap "[" "]"
                          , ppHidden = xmobarColor "#82aaff" "" . wrap "*" ""
                          , ppHiddenNoWindows = xmobarColor "#c792ea" ""
                          }
                        , handleEventHook = handleEventHook def <+> fullscreenEventHook
                        }
    `additionalKeysP` myKeys
    `removeKeysP`     ["M-<Space>"]
