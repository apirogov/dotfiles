#!/bin/sh
#command to update tracked config files..
REPO=$(cd `dirname "${BASH_SOURCE[0]}"` && pwd)/
echo $REPO
rsync --existing -rpv $HOME/ $REPO
