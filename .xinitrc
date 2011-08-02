#!/bin/sh
#
# ~/.xinitrc
# Executed by startx (run your window manager from here)
#
# exec startkde
# exec startxfce4
# exec icewm
# exec openbox
# exec blackbox
# exec fluxbox
# exec ratpoison
# exec dwm
# ... or any other WM of your choosing ...
#exec twm

#Apps before WM
xset m 1 0 & #set mouse sensitivity
#xmodmap -e "pointer = 3 2 1" & #invert mouse for left hand
setxkbmap de nodeadkeys &
nitrogen --restore &
xsetroot -cursor_name left_ptr

#Start WM (last thing to be done)
case $1 in
  openbox)
	  exec ck-launch-session openbox-session ;;
esac

#Run parallel xservers with: xinit [parameter] -- :[N]
#N=Number (default = 0, so use 1, 2, 3 etc.

#IF YOU WANT SOMETHING AFTER THE WM USE FOLLOWING:
#exec ck-launch-session windowmanager &
#wmpid=$!
#sleep 0.5
#{
#	#Apps after WM if no own config for WM is avialable
#	xcompmgr -cCfF -t-2 -l-2 -r4 -o.5 -D6 &  #add -fF for transparency
#	wicd-client &
#} &
#wait $wmpid

