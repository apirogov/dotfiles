#!/bin/sh
ip link set wlan0 up
wpa_supplicant -B -Dwext -i wlan0 -c /etc/wpa_supplicant.conf
ip addr add 192.168.1.3/24 dev wlan0
ip route add default via 192.168.1.254
