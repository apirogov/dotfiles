#!/bin/bash
# manage truecrypt containers using tcplay
usage="Usage: tcrypt open|close CONTAINERFILE"

# show supported algorithms of tcplay
if [[ "$1" == "alglist" ]]; then
  tcplay -a help
  tcplay -b help
  exit 0
fi

# arguments missing
if [[ $# -ne 2 ]]; then
  echo $usage
  exit 1
fi

# must be run as root
if (( $EUID != 0 )); then
  echo "You must be root to run this. Use sudo!"
  exit 1
fi

user=$SUDO_USER
group=$(id -g -n $user)
userid=$(id -u $user)
gid=$(id -g $user)

cryptpath=$2
cryptdev=`basename $2`
loopdev=$(losetup -f)
mountpt=/media/"$cryptdev"

# unecrypt and mount container
if [[ "$1" == "open" ]]; then
  losetup "$loopdev" "$cryptpath"
  tcplay -m "$cryptdev" -d "$loopdev"

  # read passphrase
  read -r -s passphrase <<EOF
  "$passphrase"
EOF

  # mount container
  [[ -d $mountpt ]] || mkdir -p "$mountpt"
  # mount options (uid and gid don't work with ext2/3/4!)
  mount -o nosuid /dev/mapper/"$cryptdev" "$mountpt"

# close and clean up
elif [[ "$1" == "close" ]]; then
  device=$(awk -v dev=$cryptdev -F":" '/dev/ {print $1}' <(losetup -a))
  umount "$mountpt"
  tcplay -u "$cryptdev"
  losetup -d "$device"
  rmdir /media/"$cryptdev"

# create a container file
elif [[ "$1" == "create" ]]; then
  set -e
  #if such a file exists, verify before continuing
  if [[ -e "$cryptpath" ]]; then
    read -p "File exists! Overwrite? (y/N) " -n 1 -r
    echo
    if [[ $REPLY =~ ^[^Yy]$ ]]; then
      echo "Operation aborted!"
      exit 0
    fi
  fi

  printf "Enter the container size (like 5M, 1G, etc.): "
  read size
  dd if=/dev/zero of="$cryptpath" bs=1 count=0 seek=$size
  losetup $loopdev "$cryptpath"

  #TODO: let user choose algorithms comfortably and interactively
  #Set encryption
  tcplay -c -d $loopdev -a whirlpool -b AES-256-XTS,TWOFISH-256-XTS,SERPENT-256-XTS
  tcplay -m "$cryptdev" -d $loopdev

  #TODO: let user choose file system
  #Create file system with correct permissions
  mkfs.ext3 -E root_owner=$userid:$gid /dev/mapper/"$cryptdev"

  #close and cleanup
  tcplay -u "$cryptdev"
  losetup -d $loopdev
  set +e

else #invalid command
  echo $usage
fi
