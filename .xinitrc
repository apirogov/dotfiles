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
#xset m 1 0 & #set mouse sensitivity
#xmodmap -e "pointer = 3 2 1" #invert mouse for left hand
setxkbmap de nodeadkeys      #set default layout
screens -d  #use my custom screen toggle script to set dualscreen

nitrogen --restore &
xsetroot -cursor_name left_ptr
#synclient PalmDetect=1
#synclient PalmMinWidth=2
#syndaemon -K -i 1 -d &
synclient TouchpadOff=1

#enable proxy (to use with hamachi, pidgin, dropbox, skype)
assignProxy(){
  PROXY_ENV="http_proxy ftp_proxy https_proxy all_proxy HTTP_PROXY HTTPS_PROXY FTP_PROXY ALL_PROXY"
  for envar in $PROXY_ENV
  do
    export $envar=$1
  done
}
#assignProxy "http://127.0.0.1:8888"
#assignProxy "http://141.83.42.10:3128"

#some java hacks to make minecraft etc work with openjdk7
export _JAVA_AWT_WM_NONREPARENTING=1
export LD_LIBRARY_PATH="/usr/lib/jvm/java-7-openjdk/jre/lib/amd64"

~/.autorun

#Start WM (last thing to be done)
#eval $(grep "export MEDIAHOST" .bashrc) #for keybindings in xmonad / not necessary anymore
case "$1" in
  openbox)
	  exec openbox-session ;;
  xmonad)
    exec ck-launch-session dbus-launch xmonad ;;
  *)  #default
    exec ck-launch-session dbus-launch xmonad ;;
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

