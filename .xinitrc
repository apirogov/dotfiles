#!/bin/sh
# ~/.xinitrc, executed by startx (run your window manager from here)

#From /etc/skel.d/.xinitrc
if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

#Stuff before WM
#---------------
xset +fp ~/.fonts # add local font dir
xset s off # turn off black Screensaver
xset b off # disable beep sound
#xset -dpms # turn off Display Power Management Service (DPMS)
setterm -blank 0 -powerdown 0

##Mouse:
# xset m 3 0 #set mouse sensitivity
# xmodmap -e "pointer = 3 2 1" #invert mouse for left hand
xsetroot -cursor_name left_ptr #Set cursor

##Touchpad settings
synclient HorizEdgeScroll=0 HorizTwoFingerScroll=0 #fix MEvent CASE! in java
synclient TapButton1=1 TapButton2=3 TapButton3=2   #tap-to-click as usual

#Keyboard:
setxkbmap de neo -option #set neo keyboard layout
#xmodmap -e "keycode 105 = Multi_key" #Ctrl_R -> Multi_key
#set Scroll-lock key to switch QWERTZ (default) and NEO
#setxkbmap -layout de,de -variant nodeadkeys,neo -option -option grp:sclk_toggle -option grp_led:scroll
#set Scroll-lock key to switch NEO (default) and QUERTZ
#setxkbmap -layout de,de -variant neo,nodeadkeys -option -option grp:sclk_toggle -option grp_led:scroll
#Other layouts:
#setxkbmap us cz_sk_de -option caps:escape #US layout with Umlauts on AltGr and capslock=esc
#alternative: -option ctrl:nocaps #caps lock = another ctrl
#setxkbmap de nodeadkeys

#xset -r 51 && xset -r 94 #no Mod3/4 key repeat - for Neo
#fix repetition of arrow keys (some weird Xorg regression bug, seems gone)
# xset r 113 && xset r 116

#Force GTK to use XIM to read .Xcompose
#export GTK_IM_MODULE="xim"
#export XMODIFIERS="@im=none"

#Use fcitx for unicode input
export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx
fcitx &
#----

##Essential daemons
pulseaudio --start & #autospawn is disabled for reasons
lxpolkit &           #lightweight polkit daemon
##Settings
xrdb ~/.Xresources
##Applications
~/.autorun

#Start WM (last thing to be done)
case "$1" in
  openbox)
	  exec openbox-session ;;
  xmonad)
    exec dbus-launch xmonad ;;
  *)  #default
    exec xmonad ;;
esac

#Run parallel xservers with: startx [parameter] -- :[N]
#N=Number (default = 0, so use 1, 2, 3 etc.
