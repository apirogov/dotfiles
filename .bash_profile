if [[ -z "$DISPLAY" ]] && [[ $(tty) = /dev/tty1 ]]; then
	exec xinit openbox
fi

. $HOME/.bashrc
