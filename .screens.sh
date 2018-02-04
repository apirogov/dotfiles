#!/bin/bash
# This script toggles the extended monitor outputs if something is connected
# no parameters: toggle
# -d: dual screen if possible
# anything else: single screen

DEFAULT_OUTPUT='eDP1'  # your notebook monitor
OUTPUTS='HDMI1 HDMI2'   # outputs to toggle if connected

# EXT_OPTS="--auto --above $DEFAULT_OUTPUT" # Actual settings for external monitor
EXT_OPTS_DEFAULT="--auto --pos 0x1050"
EXT_OPTS_HDMI1="--auto --rotate left --pos 1920x0" #"--pos 1080x210 "
EXT_OPTS_HDMI2="--auto --pos 240x0"

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
