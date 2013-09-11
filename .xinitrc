#!/bin/sh
#
# ~/.xinitrc
# Executed by startx (run your window manager from here)

#From /etc/skel.d/.xinitrc
if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

#Stuff before WM

#xset m 1 0 & #set mouse sensitivity
#xmodmap -e "pointer = 3 2 1" #invert mouse for left hand

#setxkbmap de nodeadkeys      #set default layout
setxkbmap us cz_sk_de

screens -d  #use my custom screen toggle script to set dualscreen

nitrogen --restore &
xsetroot -cursor_name left_ptr

synclient PalmDetect=1
synclient PalmMinWidth=5
synclient PalmMinZ=150
#syndaemon -K -i 1 -d &
synclient TouchpadOff=1

#some java hacks to make minecraft etc work with openjdk7
export _JAVA_AWT_WM_NONREPARENTING=1
export LD_LIBRARY_PATH="/usr/lib/jvm/java-7-openjdk/jre/lib/amd64"

#Daemons

#pulseaudio --start &
/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
xscreensaver -nosplash &
/usr/lib/notification-daemon-1.0/notification-daemon &

#Applications
~/.autorun

#Start WM (last thing to be done)
#eval $(grep "export MEDIAHOST" .bashrc) #for keybindings in xmonad / not necessary anymore
case "$1" in
  openbox)
	  exec openbox-session ;;
  xmonad)
    exec dbus-launch xmonad ;;
  *)  #default
    exec xmonad ;;
esac

#Run parallel xservers with: xinit [parameter] -- :[N]
#N=Number (default = 0, so use 1, 2, 3 etc.
