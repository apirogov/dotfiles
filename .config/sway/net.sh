#!/usr/bin/env bash

if [ "$1" = "list" ]; then
  echo "-"
  find /etc/netctl/ -maxdepth 1 -type f | sed 's/^.*\///' 

elif [ "$1" = "select" ]; then
  if [ "$2" = "-" ]; then
    sudo netctl stop-all
  else
    sudo netctl switch-to $2
  fi
fi
