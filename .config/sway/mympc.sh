#!/usr/bin/env bash

waybar_conf=~/.config/waybar/config.jsonc
curr_mpd_file=/tmp/currmpd

# ----

curr_mpd=$(cat $curr_mpd_file)

if [ "$1" = "cmd" ]; then
  echo $curr_mpd | xargs -Ihost mpc -h host ${@:2}

elif [ "$1" = "gui" ]; then
  term=$2
  $term -e bash -c "ncmpcpp -h $curr_mpd"

elif [ "$1" = "list" ]; then
  if [ -n "$curr_mpd" ]; then
    echo $curr_mpd
  fi
  cat ~/.mpdpwd

elif [ "$1" = "select" ]; then
  # update special tempfile
  if [ -f "$curr_mpd_file" ]; then
    rm $curr_mpd_file
  fi
  echo $2 > $curr_mpd_file

  # parse the input of the form: pwd@host
  host_pwd_arr=($(echo $2 | sed 's/@/ /'))
  pwd="${host_pwd_arr[0]}"
  server="${host_pwd_arr[1]}"
  if [ -z "$server" ]; then
    server="$pwd"
    pwd=""
  fi

  # update waybar widget (change server in config and restart waybar)
  sed -i 's/\(: \"\).*\(\", \/\/\(MPD_.*\)\)$/\1__\3__\2/' $waybar_conf
  sed -i "s/__MPD_SERVER__/$server/" $waybar_conf
  sed -i "s/__MPD_PASSWORD__/$pwd/" $waybar_conf
  killall -SIGUSR2 waybar
fi
