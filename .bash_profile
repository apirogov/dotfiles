if [[ -z "$DISPLAY" ]] && [[ $(tty) = /dev/tty1 ]]; then
	exec xinit xmonad
fi

. $HOME/.bashrc
