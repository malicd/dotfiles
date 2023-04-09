import XMonad

import Data.Map (fromList)

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.Ungrab
import XMonad.Util.SpawnOnce

import XMonad.Layout.Magnifier
import XMonad.Layout.ThreeColumns
import XMonad.Layout.IndependentScreens
import XMonad.Layout.ResizableTile

import XMonad.Hooks.EwmhDesktops

import Graphics.X11.ExtraTypes.XF86

   -- ColorScheme module (SET ONLY ONE!)
      -- Possible choice are:
      -- DoomOne
      -- Dracula
      -- GruvboxDark
      -- MonokaiPro
      -- Nord
      -- OceanicNext
      -- Palenight
      -- SolarizedDark
      -- SolarizedLight
      -- TomorrowNight
import Colors.DoomOne

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

myNormColor :: String       -- Border color of normal windows
myNormColor   = colorBack   -- This variable is imported from Colors.THEME

myFocusColor :: String      -- Border color of focused windows
myFocusColor  = color15     -- This variable is imported from Colors.THEME

myBorderWidth :: Dimension
myBorderWidth = 1           -- Sets border width for windows


myWorkspaces = ["一","二","三","四","五"]

main :: IO ()
main = xmonad
     . ewmhFullscreen
     . ewmh
     . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey
     $ myConfig

myConfig = def
    { modMask            = mod4Mask      -- Rebind Mod to the Super key
    , manageHook         = myManageHook  -- Match on certain windows
    , workspaces         = myWorkspaces
    , layoutHook         = myLayoutHook
    -- , terminal = "alacritty -e tmux"
    , terminal           = "tabbed alacritty --embed"
    , borderWidth        = myBorderWidth
    , normalBorderColor  = myNormColor
    , focusedBorderColor = myFocusColor
    , startupHook        = myStartupHook
    } `additionalKeysP` myKeys

myLayoutHook = Full ||| tall ||| Mirror tall
   where
  -- Two master panes, 1/10th resize increment, only show master
  -- panes by default. Unlike plain 'Tall', this also allows
  -- resizing the master panes, via the 'MirrorShrink' and
  -- 'MirrorExpand' messages.
  tall = ResizableTall 2 (1/10) 1 []

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "clipmenud"
  spawnOnce "dunst --startup_notification true"

myKeys =
    [ ("M-S-z"            , spawn "xscreensaver-command -lock"                                   )
    , ("M-C-s"            , unGrab *> spawn "scrot -s"                                           )
    -- , ("M-h"              , spawn "alacritty -e clipcat-menu --finder=builtin edit --editor vim" )

    , ("M-f"              , spawn "firefox"                                                      )
    , ("M-n"              , spawn "nautilus"                                                     )

    , ("<XF86AudioMute>"            , spawn "amixer -D pulse sset Master toggle"                           )
    , ("<XF86AudioRaiseVolume>"    , spawn "amixer -D pulse sset Master 5%+"                              )
    , ("<XF86AudioLowerVolume>"  , spawn "amixer -D pulse sset Master 5%-"                              )
    , ("<XF86AudioPrev>"              , spawn "playerctl -a previous"                                        )
    , ("<XF86AudioPlay>"              , spawn "playerctl -a play-pause"                                      )
    , ("<XF86AudioNext>"              , spawn "playerctl -a next"                                            )

    , ("M-<Left>"      , sendMessage MirrorExpand                                            )
    , ("M-<Up>"        , sendMessage MirrorExpand                                            )
    , ("M-<Right>"     , sendMessage MirrorShrink                                            )
    , ("M-<Down>"      , sendMessage MirrorShrink                                            )
    ]
    ++
    -- change M-{w,e,r} order to match the screen order
    [ (mask ++ "M-" ++ [key], screenWorkspace scr >>= flip whenJust (windows . action))
         | (key, scr)  <- zip "wer" [0,2,1] 
         , (action, mask) <- [ (W.view, "") , (W.shift, "S-")]
    ]

myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Gimp"        --> doFloat
    , className =? "copyq"       --> doFloat
    , isDialog                   --> doFloat
    , className =? "thunderbird" --> doShift "msg"
    , className =? "Mattermost"  --> doShift "msg"
    ]

myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = yellow " • "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = wrap " " "" . xmobarBorder "Top" myFocusColor 2
    , ppHidden          = white . wrap " " ""
    , ppVisible         = white . wrap "["  "]"
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, _, _, wins] -> [ws, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (white    "[") (white    "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow

    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 10

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor "#bd93f9" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""
