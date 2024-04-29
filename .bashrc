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
shopt -s cdspell    # This will correct minor spelling errors in a cd command.
shopt -s histappend # Append to history rather than overwrite
shopt -s checkwinsize   # Check window after each command
shopt -s dotglob    # files beginning with . to be returned in the results of path-name expansion.
shopt -s extglob    # allows e.g. negative pattern matching !(*foo)
shopt -s globstar   # allows ** for recursive globbing
shopt -s autocd     # auto-cd if entering a path

## SET OPTIONS
set -o ignoreeof    # stops ctrl+d from logging me out
set -o noclobber    # prevent overwriting files with cat
# set -o vi           # vi input mode

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
# export EDITOR="emacsclient -t"
# export ALTERNATE_EDITOR=""
# export VISUAL="emacsclient -c -n"
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
alias findtext="grep --color=always -ri" #better use ag when installed
alias findfile='find . -name'
alias ping='ping -c 5'	#Limit ping number
alias pingl='ping6 ff02::1%eth0'  #ping local devices

#Package Management - ArchLinux Pacman
if which pacman >/dev/null; then
  pss(){ aura -Ss $@; aura -As $@; } #Search for packages
  pin(){ sudo -- sh -c "pacman -S --needed $@ || aura -A $@"; } #Install package
  pgl(){ pacman -Qqnei | grep -E "(Name)|(Beschreibung)" | tr -d '\n' | sed 's/Name\s*: /\n/g' | sed 's/Beschreibung\s*: /\t/'; }    #List of installed packages
  alias pup='sudo -- sh -c "aura -Syu; aura -Au"'   #Dist upgrade
  alias prp='sudo pacman -Ruscn' #Recursive remove
  alias pil='sudo pacman -U'     #Install local Package
  alias pro='sudo aura -Oj'      #List and remove orphans
  alias pgf='pacman -Qqme'       #List foreign/AUR packages
  alias pql='pacman -Ql'         #List files installed by that package
fi

if which systemctl >/dev/null; then
  alias listd="systemctl list-unit-files --type=service" #show all daemons run on startup
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

calc(){ awk "BEGIN{print $*}"; } # calculator
joinl(){ paste $(for i in $(seq 1 $2); do echo -n " - "; done) -d"$1"; }
permutate(){ echo $@ | sed 's/ /\n/g' | shuf; }

# if which stack >/dev/null; then #define these if stack is present
#   eval "$(stack --bash-completion-script stack)"
#   # alias ghc="stack ghc --"
#   # ghci(){ if [ -e stack.yaml ]; then stack ghci --no-build; else stack ghci --no-build --ghci-options "-ghci-script $HOME/.ghcii"; fi; }
# fi

#Music
alias setmp3chmod='find -name "*.mp3" -print0 | xargs -0 chmod 644'
alias fixmusicdir='chmod -R u+rwX,go+rX,go-w ./'  #set files to 644, dirs to 755
alias m4a2mp3='for a in *.m4a; do faad -f 2 -w "$a"  | lame -r - "$a.mp3"; done'
alias flac2mp3='for a in ./*.flac; do ffmpeg -i "$a" -qscale:a 0 "${a[@]/%flac/mp3}"; done'
alias wav2mp3='for i in *.wav; do lame -h -b 320 "$i" "`basename "$i" .wav`".mp3; done'
alias normalizevolume='find /home/admin/myfiles/music/ -type f -iname "*.mp3" -exec mp3gain -p -r -k -s i -d 6.0 "{}" \;'
# cutflac() { shnsplit -f "$1" "$2" && wav2mp3 && cuetag.sh "$1" *.mp3 && rm -f *.wav; }
alias cutflac='shnsplit -f *.cue -o flac *.flac && cuetag.sh *.cue split-track*.flac'
to_monowav() { sox $1 $2 channels 1 rate 44100; }

#----
#mediacenter - using ssh/sshfs/rsync/mpd/ncmpcpp
#use to: backup folders, mount remote data, control music
#requires to have MPD default port, MEDIASSHP for ssh and port 8000 for mpd streaming open on media server
MEDIAUSER=admin       #user for ssh connection
MPDPWDFILE=~/.mpdpwd  #.mpdpwd, which should contain PWD@HOST per line
MEDIASSHP=2200        #SSH port
MEDIAHOST=$(head -n 1 $MPDPWDFILE | sed 's/.*@//') #takes first host from list for following aliases
MEDIALOGIN=$MEDIAUSER@$MEDIAHOST
REMOTE_DATA_DIR=$MEDIALOGIN:/media/DATA
alias center.mount="mkdir ~/media; sshfs -p $MEDIASSHP $REMOTE_DATA_DIR ~/media"
alias center.umount="fusermount -u ~/media; rmdir ~/media"
alias center.mpc="ncmpcpp -h $MEDIAHOST"
alias center.stream="mpv http://$MEDIAHOST:8000"
#mirror local -> remote external
function center.updatebackup() {
  LOCAL_DATA_DIR=~/myfiles
  RSH_CMD="ssh -p $MEDIASSHP"
  RSYNC_FILT_ARGS="-aup -FF"
  RSYNC_ARGS="--info=progress2 --delete"
  rsync-prepare -v -e "$RSH_CMD" -f "$RSYNC_FILT_ARGS" $LOCAL_DATA_DIR $REMOTE_DATA_DIR
  rsync --timeout 180 -v -e "$RSH_CMD" $RSYNC_ARGS $RSYNC_FILT_ARGS  $LOCAL_DATA_DIR $REMOTE_DATA_DIR
}

#alias to access either first mpd from .mpdpwd -> mediaserver, or fallback to localhost
SMARTMPD='$(if ping -c 1 -w 1 $MEDIAHOST > /dev/null; then echo $(head -n 1 $MPDPWDFILE); else echo localhost; fi)'
alias music="ncmpcpp -h $SMARTMPD"
alias local.updatebackup="udevil mount /dev/disk/by-label/HDDDATA /media/backup; rsync --info=progress2 --delete -auv /home/admin /media/backup/; udevil umount /media/backup" #mirror local -> external

#----

#Go to ranger bookmark
cdm() { if [[ "$1" != "" ]]; then cd $(grep "^$1" ~/.config/ranger/bookmarks | sed "s/^$1://"); fi; }
#Same as bash completion - cdm <letter><TAB>
_cdm() {
  local cur=${COMP_WORDS[COMP_CWORD]}
  if [[ "$cur" = "" ]]; then
    COMPREPLY=($(cat ~/.config/ranger/bookmarks | sed "s/:\/.\+//"))
  else
    local p=$(grep "^$cur" ~/.config/ranger/bookmarks | sed "s/^$cur://")
    if [[ "$p" = "" ]]; then
      COMPREPLY=()
    else
      COMPREPLY=("; cd $p/")
    fi
  fi
}
complete -o nospace -F _cdm cdm

source ~/.bashrc_extras
# ----

export PYENV_ROOT="$HOME/.pyenv"
command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"

[ -f "/home/admin/.ghcup/env" ] && source "/home/admin/.ghcup/env" # ghcup-env
source /usr/share/nvm/init-nvm.sh
