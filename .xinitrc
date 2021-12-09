#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps

if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"

fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# start some nice programs

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

numlockx &

xrandr --output DP1 --auto --scale 0.9999x0.9999 --output HDMI2 --auto --scale 1.5x1.5 --right-of DP1 --set "Broadcast RGB" "Full"
# xrandr --output DP1 --auto --output HDMI2 --auto --right-of DP1 --set "Broadcast RGB" "Full"

xsetroot -cursor_name left_ptr

session=${1:-xmonad}

case $session in
  dwm    ) exec dwm;;
  i3     ) exec i3;;
  xmonad ) exec xmonad;;
  qtile  ) exec qtile start;;
  *      ) exec $1;;
esac

