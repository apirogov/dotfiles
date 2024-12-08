#!/usr/bin/env bash
# This script is an improved and generalized version over using the following the sway config:
# (the problem is that it does not automatically re-launch the terminal if closed)
# ----
# set $scratch_term "alacritty --class popup_term --option window.opacity=0.8 -e bash -c 'zellij attach -c scratch'"
# # run scratchpad terminal
# bindsym $mod+Shift+v exec $scratch_term
# # make sure it is put into the scratchpad
# for_window [app_id="^popup_term"] floating enable, border none, mark popup_term, move scratchpad, sticky enable
# # bring it up with $mod+v
# bindsym $mod+v [con_mark="^popup_term$"] scratchpad show, resize set width 100 ppt height 50 ppt, move position 0 ppt 0 ppt
# ----
#
# NOTE: to use this script, the config should contain something like this:
# ----
# bindsym $mod+v exec ~/.config/sway/scratch_util.sh
# ----

cmd_term="alacritty --class popup_term --option window.opacity=0.8"  # " -e bash -c 'zellij attach -c scratch'"
cmd_pic="feh --class popup_pic ~/.config/sway/neo-druckvorlage.png"

function has_scratch() { swaymsg -t get_tree | grep popup_$1 > /dev/null; }

if ! has_scratch $1; then
  cmd_var="cmd_$1"
  cmd="${!cmd_var}"
  swaymsg exec "$cmd"
  while ! has_scratch $1; do sleep 0.1; done
  swaymsg for_window [app_id="^popup_$1"] floating enable, border none, mark popup_$1, move scratchpad, sticky enable
fi

swaymsg [con_mark="^popup_$1"] scratchpad show, resize set width 100 ppt height 50 ppt, move position 0 ppt 0 ppt
