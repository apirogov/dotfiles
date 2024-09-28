#!/bin/bash
# This script toggles the extended monitor outputs if something is connected
# no parameters: toggle
# -d: dual screen if possible
# anything else: single screen

DEFAULT_OUTPUT='eDP1'  # your notebook monitor
OUTPUTS='HDMI1 HDMI2'   # outputs to toggle if connected

EXT_OPTS_DEFAULT="--auto --pos 1080x2160"
# EXT_OPTS_HDMI1="--auto --rotate left --pos 1920x0"
# EXT_OPTS_HDMI2="--auto --pos 0x0"
EXT_OPTS_HDMI1="--auto --pos 0x1000 --rotate left"
EXT_OPTS_HDMI2="--mode 3840x2160 --pos 1080x0 --rotate normal"


XRANDR=`xrandr` #get info
EXECUTE="" #xrandr execution string (to be built)
for CURRENT in $OUTPUTS; do
        if [[ $XRANDR == *$CURRENT\ connected* ]]; then # is connected
                optvar="EXT_OPTS_$CURRENT"
                if [[ "$1" == "" ]] # no parameters given -> toggle
                then
                        echo "Toggling screen $CURRENT!"
                        if [[ $XRANDR == *$CURRENT\ connected\ \(* ]]; then # is disabled
                                EXECUTE+=" --output $CURRENT ${!optvar} "
                        else
                                EXECUTE+=" --output $CURRENT --off "
                        fi
                else
                        if [[ "$1" == "-d" ]]; then #explicitly dual screen
                                echo "Enabling $CURRENT!"
                                EXECUTE+=" --output $CURRENT ${!optvar} "
                        else
                                echo "Disabling $CURRENT!"
                                EXECUTE+=" --output $CURRENT --off "
                        fi
                fi
        else # make sure disconnected outputs are off
                EXECUTE+=" --output $CURRENT --off "
        fi
done

#set monitor modes:
xrandr --output $DEFAULT_OUTPUT $EXT_OPTS_DEFAULT --primary $EXECUTE
