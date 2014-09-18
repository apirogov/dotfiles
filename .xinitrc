#!/bin/sh
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
#---------------

#Mouse:

# xset m 3 0 #set mouse sensitivity
# xmodmap -e "pointer = 3 2 1" #invert mouse for left hand

#Touchpad settings
synclient PalmDetect=1
synclient PalmMinWidth=5
synclient PalmMinZ=100
syndaemon -t -K -i 1 -d
# synclient TouchpadOff=1

#Disable trackpoint
xinput set-prop "TPPS/2 IBM TrackPoint" "Device Enabled" 0

#Set cursor
xsetroot -cursor_name left_ptr

#Keyboard:

setxkbmap de neo -option #set neo keyboard layout
#set Scroll-lock key to switch QWERTZ (default) and NEO
#setxkbmap -layout de,de -variant nodeadkeys,neo -option -option grp:sclk_toggle -option grp_led:scroll
#set Scroll-lock key to switch NEO (default) and QUERTZ
#setxkbmap -layout de,de -variant neo,nodeadkeys -option -option grp:sclk_toggle -option grp_led:scroll

#Other layouts:
#setxkbmap us cz_sk_de -option caps:escape #US layout with Umlauts on AltGr and capslock=esc
#alternative: -option ctrl:nocaps #caps lock = another ctrl
#setxkbmap de nodeadkeys

xset -r 51 && xset -r 94 #no Mod3/4 key repeat - for Neo
#fix repetition of arrow keys (some weird Xorg regression bug)
# xset r 113 && xset r 116

#----

screens -d  #use my custom screen toggle script to set dualscreen

#some java hacks to make minecraft etc work with openjdk7
# export _JAVA_AWT_WM_NONREPARENTING=1
# export LD_LIBRARY_PATH="/usr/lib/jvm/java-7-openjdk/jre/lib/amd64"

#Daemons
#----
#pulseaudio --start &
/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
/usr/lib/notification-daemon-1.0/notification-daemon &
xscreensaver -nosplash &
#eval $(gpg-agent --daemon) &

#Settings
xrdb ~/.Xresources

#Applications
~/.autorun

#Start WM (last thing to be done)
case "$1" in
  openbox)
	  exec openbox-session ;;
  xmonad)
    exec dbus-launch xmonad ;;
  *)  #default
    exec dbus-launch xmonad ;;
esac

#Run parallel xservers with: startx [parameter] -- :[N]
#N=Number (default = 0, so use 1, 2, 3 etc.
