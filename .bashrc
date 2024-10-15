#Anton Pirogov's .bashrc
[ -r /etc/profile ] && . /etc/profile

#Add own binaries
paths=(~/bin ~/.local/bin ~/bin/node_modules/.bin ~/.cargo/bin)
for bindir in ${paths[@]}; do [ -d $bindir ] && PATH=${bindir}:$PATH; done

#add non-system .so libs
libs=()
for libdir in ${libs[@]}; do [ -d $libdir ] && LD_LIBRARY_PATH=$LD_LIBRARY_PATH:${libdir}; done
export LD_LIBRARY_PATH

[ -z "$PS1" ] && return # If not running interactively, we're done here.

#X Terminal titles
export PROMPT_COMMAND=""
case "$TERM" in
 xterm*|rxvt*)
  PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD/$HOME/~}\007";'
  ;;
esac

## SHOPT OPTIONS
shopt -s cdspell        # This will correct minor spelling errors in a cd command.
shopt -s histappend     # Append to history rather than overwrite
shopt -s checkwinsize   # Check window after each command
shopt -s dotglob        # files beginning with . to be returned in the results of path-name expansion.
shopt -s extglob        # allows e.g. negative pattern matching !(*foo)
shopt -s globstar       # allows ** for recursive globbing
shopt -s autocd         # auto-cd if entering a path

## SET OPTIONS
set -o ignoreeof    # stops ctrl+d from logging me out
set -o noclobber    # prevent overwriting files with cat

## BIND OPTIONS
bind 'set completion-ignore-case on'
bind 'set show-all-if-ambiguous on'
#check key combinations with read command to bind them
bind '"\e[1;3D": backward-word' # M-Left -> move a word back
bind '"\e[1;3C": forward-word'  # M-Right -> move a word forward
stty -ixon #so as not to be disturbed by Ctrl-S ctrl-Q in terminals and be able to map these in vim

## COMPLETION
complete -cf sudo    #complete stuff after sudo
complete -cf man     #complete man pages
complete -f -X '*.@(aux|fdb_latexmk|fls|pdf|out|log|class|o)' vim #don't complete these files for vim
#Btw: Alt-Backspace deletes word, CTRL-<Arrow> moves wordwise, !!, !CMD runs last matching cmd

#XTerm Escape Sequences for text color + attributes
#Change color: \e[Nm
#  For Use in PS1 put this into \[ and \] otherwise there will be strange side effects! so: \[e[Nm\]
#values for N:
#  0 default	1 bold	4 underline	5 blink	7 inverse	8 invisible
#  22 unbold	24 not underline	25 not blink	27 not inverse	28 visible
#Default 8 system colors: N=XY where:
#  X: 3=FG 4=BG | Y: 0=Black, 1=Red, 2=Green, 3=Yellow, 4=Blue, 5=Magenta, 6=Cyan, 7=White
#  Bold+Color = brighter color (for FG)
#Hint: You can combine the above numbers in one escape sequence
#  separating the numbers with semicolons, e.g. \e[1;34m for bold blue FG
#Hint: If you want the REAL ESCAPE SEQUENCE, press CTRL+V,ESC
#  (you use \e or \033 which has to be transformed with echo -e)
#256 Color mode (if supported) these special combinations:
#  FG color to index X: 38;5;X | BG color to index X: 48;5;X | X = number 0 to 255

#Default PS1: PS1='\u@\h:\W\$ '
#Colored PS1:
DEFAULT="\[\e[0m\]"; RED="\[\e[1;31m\]"; BLUE="\[\e[1;34m\]"; CYAN="\[\e[1;36m\]"; WHITE="\[\e[1;37m\]"; BLUEBG="\[\e[44m\]"
export PS1="$BLUEBG$WHITE[\t]$DEFAULT $([ "$EUID" != "0" ] && echo "$BLUE\u$RED@$CYAN" || echo "$RED\u@")\h$DEFAULT \W$WHITE \\$ $DEFAULT"

## EXPORTS
export LANG="de_DE.UTF-8"
export LC_ALL="de_DE.UTF-8"
export LANGUAGE="de_DE:en_US:en"
export TZ="Europe/Berlin"

export HISTCONTROL="ignorespace:ignoredups:erasedups" #No duplicates in history, no cmds preceded by space
export HISTIGNORE="&:ls:ll:la:q:e:pwd:exit:clear"	#Never log these
export HISTSIZE="9999"	#Lines saved in one session
export HISTFILESIZE="999999"	#Lines total
export PROMPT_COMMAND+="history -a; history -n" #share history between terminals

export BROWSER="firefox"
export TERM=xterm-256color
export EDITOR="vim"
export VISUAL="gvim"
export PAGER="less"
export LESS="-iMn -F -X -R"
export TEXMFHOME="~/.texmf"   #custom packages
# export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel'

## ALIASES AND FUNCTIONS
alias ls='ls --color=auto'
alias la='ls -A'
alias ll='ls -lhtA'
alias q='exit'
alias rm='rm -iv'       #safety - ask before delete/overwrite
alias cp='cp -iv'
alias mv='mv -iv'
alias mkdir='mkdir -p -v' #Allow multiple levels at once
alias ps='ps -e -o pid,user,comm,args,vsize,pcpu' #Tweaked process list
alias df='df -hT'       #-h : Human readable
alias du='du -sh'
alias duf='du -sk * | sort -n | perl -ne '\''($s,$f)=split(m{\t});for (qw(K M G)) {if($s<1024) {printf("%.1f",$s);print "$_\t$f"; last};$s=$s/1024}'\' #readable + sorted
alias top10size='find . -printf "%s %p\n"|sort -nr|head'
alias pingl='ping6 ff02::1%eth0'  #ping local devices
alias listd="systemctl list-unit-files --type=service" #show all daemons run on startup

#Package Management - ArchLinux Pacman
if which pacman >/dev/null; then
  pss(){ aura -Ss $@; aura -As $@; } #Search for packages
  pin(){ sudo -- sh -c "pacman -S --needed $@" || aura -A $@; } #Install package
  pgl(){ pacman -Qqnei | grep -E "(Name)|(Beschreibung)" | tr -d '\n' | sed 's/Name\s*: /\n/g' | sed 's/Beschreibung\s*: /\t/'; }    #List of installed packages
  alias pup='sh -c "aura -Syu; aura -Au"'   #Dist upgrade
  alias prp='sudo pacman -Ruscn' #Recursive remove
  alias pil='sudo pacman -U'     #Install local Package
  alias pro='aura -Oj'           #List and remove orphans
  alias pgf='pacman -Qqme'       #List foreign/AUR packages
  alias pql='pacman -Ql'         #List files installed by that package
fi

#SSH / Remote stuff
alias initsshkeys=''
if which keychain >/dev/null; then #SSH key management
  alias initsshkeys='eval `keychain --eval --nogui -Q -q id_rsa id_ed25519 2>/dev/null`'
  git() { #Make sure ssh-agent is running with keychain before using git push
    if [ "push" == "$1" ] || [ "pull" == "$1" ] || [ "clone" == "$1" ]; then
      eval `keychain --eval --nogui -Q -q id_rsa 2>/dev/null`
    fi
    $(which git) "$@"
  }
fi
alias ssh='initsshkeys && ssh'
alias sshfs='initsshkeys && sshfs -o allow_other,default_permissions'

function broken_symlinks() {
  for lnk in $(find . -type l ! -exec test -e {} \; -print); do
    echo "$lnk" "->" $(readlink -f $lnk)
  done
}

#----
#mediacenter - using ssh/sshfs/rsync/mpd/ncmpcpp
#use to: backup folders, mount remote data, control music
#requires to have MPD default port and MEDIASSHP for ssh
MEDIASSHP=2200
MEDIALOGIN=admin@mediacenter
REMOTE_DATA_DIR=$MEDIALOGIN:/media/DATA
alias center.mount="mkdir ~/media; sshfs -p $MEDIASSHP $REMOTE_DATA_DIR ~/media"
alias center.umount="fusermount -u ~/media; rmdir ~/media"
#mirror local -> remote external
function center.updatebackup() {
  LOCAL_DATA_DIR=~/myfiles
  RSH_CMD="ssh -p $MEDIASSHP"
  RSYNC_FILT_ARGS="-auvlp -FF"
  RSYNC_ARGS="--info=progress2 --delete"
  rsync-prepare -v -e "$RSH_CMD" -f "$RSYNC_FILT_ARGS" $LOCAL_DATA_DIR $REMOTE_DATA_DIR
  rsync --timeout 180 -v -e "$RSH_CMD" $RSYNC_ARGS $RSYNC_FILT_ARGS  $LOCAL_DATA_DIR $REMOTE_DATA_DIR
}

if [ -f ~/private/.restic-env ]; then
  source ~/private/.restic-env
  for repo in myfiles irfiles media; do
    alias restic-$repo="restic -r $RESTIC_REPOSITORY_BASE/$repo"
  done
fi

function t460.update() {
  if [ -z "$1" ]; then
    echo "Missing directory"
    return 1
  fi
  rsync --info=progress2 -auvlp -FF --delete -e "ssh -p 2200" /home/anton/$1/ admin@192.168.1.30:/home/admin/$1/
}

if [ -f ~/.bashrc_extras ]; then source ~/.bashrc_extras; fi

#----

function work() {
  luks_partition_name=SECURE
  work_dir=~/work
  work_bashrc=$work_dir/utils/bashrc_work
  wg_iface=work-vpn
  wg_config=$work_dir/wireguard/$wg_iface.conf

  if [ "$1" = "start" ]; then
    work mount
    work connect
    work source-bashrc
  elif [ "$1" = "stop" ]; then
    work disconnect
    work umount

  elif [ "$1" = "mount" ]; then
    # NOTE: to change password, use cryptsetup luksChangeKey /dev/ENCRYPTED_DEVICE
    sudo cryptsetup open /dev/disk/by-partlabel/$luks_partition_name work_data
    sudo mount -m /dev/mapper/work_data $work_dir
  elif [ "$1" = "umount" ]; then
    sudo umount $work_dir
    rmdir $work_dir
    sudo cryptsetup close /dev/mapper/work_data

  elif [ "$1" = "source-bashrc" ]; then
    [ -f "$work_bashrc" ] && source $work_bashrc

  elif [ "$1" = "connected" ]; then
    ip a | grep $wg_iface > /dev/null
  elif [ "$1" = "connect" ]; then
    sudo wg-quick up $wg_config
  elif [ "$1" = "disconnect" ]; then
    sudo wg-quick down $wg_config

  elif [ "$1" = "zellij" ]; then
    # NOTE: WORKSTATION_HOST is set in the work bashrc
    ! work connected && work connect
    ssh -t $WORKSTATION_HOST "zellij attach -c work"
  else
    echo "unknown command!"
    return 1
  fi
}

work source-bashrc

# ----
export PYENV_ROOT="$HOME/.pyenv"
command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"
