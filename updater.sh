#!/bin/sh
#command to update tracked config files..
REPO=$(cd `dirname "${BASH_SOURCE[0]}"` && pwd)/
rsync --existing -auv /home/admin/* $REPO
