#Anton Pirogov's .bashrc
. /etc/profile

#Add own binaries
#Global cabal sandbox only for recent cabal-install and xmonad
paths=(~/bin ~/bin/matlab/bin ~/.cabal/bin ~/bin/sandboxes/*/bin ~/.gem/ruby/*/bin)

for bindir in ${paths[@]}; do
    if [ -d $bindir ]; then
        PATH=$PATH:${bindir}
    fi
done

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

#X Terminal titles
export PROMPT_COMMAND=""
case "$TERM" in
 xterm*|rxvt*)
  PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD/$HOME/~}\007";'
  ;;
 *)
  ;;
esac

## SHOPT OPTIONS
shopt -s cdspell	# This will correct minor spelling errors in a cd command.
shopt -s histappend	# Append to history rather than overwrite
shopt -s checkwinsize	# Check window after each command
shopt -s dotglob	# files beginning with . to be returned in the results of path-name expansion.
shopt -s extglob    # allows e.g. negative pattern matching !(*foo)

## SET OPTIONS
set -o ignoreeof    # stops ctrl+d from logging me out
#Btw: Alt-Backspace deletes word in emacs mode
#set -o vi           #vi mode
#set -o noclobber    # prevent overwriting files with cat

## BIND OPTIONS
bind 'set completion-ignore-case on'
bind 'set show-all-if-ambiguous on'

#XTerm Escape Sequences for text color + attributes
#Change color: \e[Nm
#For Use in PS1 put this into \[ and \] otherwise there will
#be strange side effects! so: \[e[Nm\]
#
#values for N:
#0 default	1 bold	4 underline	5 blink	7 inverse	8 invisible
#22 unbold	24 not underline	25 not blink	27 not inverse	28 visible
#Default 8 system colors: N=XY where:
#X: 3=FG 4=BG
#Y: 0=Black, 1=Red, 2=Green, 3=Yellow, 4=Blue, 5=Magenta, 6=Cyan, 7=White
#Bold+Color = brighter color (for FG)
#Hint: You can combine the above numbers in one escape sequence
#  separating the numbers with semicolons, e.g. \e[1;34m for bold blue FG
#Hint: If you want the REAL ESCAPE SEQUENCE, press CTRL+V,ESC
#(you use \e or \033 which has to be transformed with echo -e)
#
#256 Color mode (if supported) these special combinations:
#FG color to index X: 38;5;X
#BG color to index X: 48;5;X
#X = number 0 to 255

#Default PS1:
#PS1='\u@\h:\W\$ '
#Colored PS1:
DEFAULT="\[\e[0m\]"
RED="\[\e[1;31m\]"
GREEN="\[\e[32m\]"
GGREEN="\[\e[1;32m\]"
BLUE="\[\e[1;34m\]"
CYAN="\[\e[1;36m\]"
WHITE="\[\e[1;37m\]"
BLUEBG="\[\e[44m\]"
export PS1="$BLUEBG$WHITE[\t]$DEFAULT $([ "$EUID" != "0" ] && echo "$BLUE\u$RED@$CYAN" || echo "$RED\u@")\h$DEFAULT \W$WHITE \\$ $DEFAULT" 

#EXPORTS
export LANG="de_DE.UTF-8"
export LC_ALL="de_DE.UTF-8"
export TZ="Europe/Berlin"

export HISTCONTROL="ignorespace:erasedups" #No duplicates in history, no cmds preceded by space
export HISTIGNORE="&:ls:ll:la:pwd:exit:clear"	#Never log these
export HISTSIZE="9999"	#Lines saved in one session
export HISTFILESIZE="999999"	#Lines total
export PROMPT_COMMAND+="history -a; history -n" #share history between terminals

export BROWSER="firefox"
export EDITOR="vim"
export VISUAL="gvim"
export PAGER="less"
export LESS="-iMn -F -X -R"

#ALIASES
#navigation
alias ls='ls --color=auto'
alias la='ls -A'
alias ll='ls -lhtA'

#Package Management - Pacman
alias ygl='pacman -Qqne'      #List of installed packages
alias ygf='pacman -Qqme'      #List foreign/AUR packages
alias ysi='pacman -Si'				#Search Info about packages
alias yss='yaourt -Ss'				#Search for packages
alias yip='sudo pacman -U'		#Install local Package
alias yin='sudo pacman -S --needed'	#Install package from database
alias yrm='sudo pacman -Ruscn'		#Recursive remove
alias yup='yaourt -Syu --aur'		#Dist upgrade

#file operation/standard tools
alias ..='cd ..'
alias q='exit'
alias df='df -hT'		#-h : Human readable
alias du='du -sh'
alias duf='du -sk * | sort -n | perl -ne '\''($s,$f)=split(m{\t});for (qw(K M G)) {if($s<1024) {printf("%.1f",$s);print "$_\t$f"; last};$s=$s/1024}'\' #readable + sorted
alias rm='rm -iv'		#safety - ask before delete/overwrite
alias cp='cp -iv'
alias mv='mv -iv'
alias mkdir='mkdir -p -v'	#Allow multiple levels at once
alias sym='ln -s'		#Symlink
alias ps='ps -e -o pid,comm,args,vsize,pcpu' #Tweaked process list
alias findfile='find . -name'
alias findtext="grep -ri" #better use ag when installed
alias cleanhistory="tac ~/.bash_history | awk '!seen[$0]++' | tac > newhist"
alias top10size='find . -printf "%s %p\n"|sort -nr|head'
alias showswap='cat /proc/swaps'
alias listd="systemctl list-unit-files --type=service" #show all daemons run on startup

#SSH key
alias initsshkeys='eval `keychain --eval --nogui -Q -q id_rsa`'
alias ssh='initsshkeys && ssh'

#Misc. Programs
alias xp='xprop | grep "WM_WINDOW_ROLE\|WM_CLASS" && echo "WM_CLASS(STRING) = \"NAME\", \"CLASS\"";xwininfo'
alias tmux='tmux -2'
alias t='todo.sh'
alias tcrypt='sudo ~/.tcrypt.sh'

#Map my cheap generic gamepad to XBox controller API
alias fakexbox='xboxdrv --evdev-absmap ABS_RX=X2,ABS_RZ=Y2,ABS_X=X1,ABS_Y=Y1,ABS_HAT0X=dpad_x,ABS_HAT0Y=dpad_y --axismap -Y1=Y1,-Y2=Y2 --evdev-keymap BTN_THUMB2=a,BTN_THUMB=b,BTN_TOP=x,BTN_TRIGGER=y,BTN_BASE4=start,BTN_BASE3=back,BTN_TOP2=lt,BTN_BASE=lb,BTN_PINKIE=rt,BTN_BASE2=rb --deadzone 10% --mimic-xpad --silent --evdev '

#Compiler settings
alias hc='rm -rf /tmp/*.o; ghc -Wall -fwarn-name-shadowing -fwarn-incomplete-patterns -hidir=/tmp -odir=/tmp -O' #"script compile" shortie
alias gcc='LANG="C" gcc -ansi -std=c99 -pedantic -Wall -Wextra -Wshadow -Wcast-qual -Wformat=2 -Wmissing-include-dirs -Wfloat-equal -Wswitch-enum -Wundef -Wwrite-strings -Wredundant-decls -fverbose-asm -pg -g '	#High standard level, many debugging opts

#Internet
alias ping='ping -c 5'	#Limit ping number
alias pingl='ping6 ff02::1%eth0'  #ping local
alias getip="wget -O - -q http://checkip.dyndns.org/index.html|sed -e 's/.* //' -e 's/<.*//'"
alias speedtest='wget -O /dev/null http://speedtest.wdc01.softlayer.com/downloads/test10.zip'

#Filesystem
alias mkisofs='mkisofs -v -r -J -o'	#Usage: mkisofs target.img /src/path
alias mirror="rsync -auv --delete"

#PDF
alias joinpdf='gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=join.pdf ' #requires ghostscript, convert also works!
alias makepdf='pdflatex *.tex && evince *.pdf' #shortie for iterations with latex homeworks

#Uni
alias pushpoolprint='initsshkeys; ssh sshgate mv ./print/*.pdf ./print/old; scp *.pdf sshgate:./print/ ; echo "Inhalt print:" ; ssh sshgate ls print'
alias sshfs='sshfs -o idmap=user'

#Music
alias setmp3chmod='find -name "*.mp3" -print0 | xargs -0 chmod 644'
alias fixmusicdir='chmod -R u+rwX,go+rX,go-w ./'  #set files to 644, dirs to 755
alias m4a2mp3='for a in *.m4a; do faad -f 2 -w "$a"  | lame -r - "$a.mp3"; done'
alias normalizevolume='find /home/admin/myfiles/music/ -type f -iname "*.mp3" -exec mp3gain -p -r -k -s i -d 6.0 "{}" \;'

#mediacenter - using ssh/sshfs/rsync/mpd/ncmpcpp
#use to: backup folders, mount remote data, control music
#requires to have MPD default port, MEDIAPORT for ssh and port 8000 for mpd streaming open on media server
#MEDIAHOST "mediacenter" must be declared correctly in /etc/hosts on both machines
#alternative with less available ports: use ssh tunnel like:
#ssh -p MEDIAPORT -fN MEDIALOGIN -L LOCALPORT:MEDIAHOST:REMOTEPORT
#and connect to localhost to these ports
MEDIAUSER=admin       #user for ssh connection
MEDIAHOST=mediacenter #declared hostname or IP
MPDPWDFILE=~/.mpdpwd
MEDIASSHP=2200  #SSH port
MEDIALOGIN=$MEDIAUSER@$MEDIAHOST
alias center.ssh="ssh -p $MEDIASSHP $MEDIALOGIN"
alias center.ping="ping -c 5 $MEDIAHOST"
alias center.mount="mkdir ~/media; sshfs -p $MEDIASSHP $MEDIALOGIN:/media/DATA ~/media"
alias center.umount="fusermount -u ~/media; rmdir ~/media"
alias center.updatebackup="rsync --delete -avue 'ssh -p $MEDIASSHP' ~/myfiles $MEDIALOGIN:/media/DATA" #mirror local -> remote external
alias center.mpc="ncmpcpp -h $MEDIAHOST"
alias center.stream="mplayer -nocache http://$MEDIAHOST:8000"

#alias to access best accessible mpd -> mediaserver or fallback localhost
SMARTMPD='$(if ping -c 1 -w 1 $MEDIAHOST > /dev/null; then echo $(cat $MPDPWDFILE)@$MEDIAHOST; else echo localhost; fi)'
alias music="ncmpcpp -h $SMARTMPD"

#Hardware control
alias cdo='eject sr0'	#CD Open
alias cdc='eject -t sr0' 	#CD Close
alias togglepad='killall syndaemon; synclient TouchpadOff=$(synclient -l | grep -c "TouchpadOff.*=.*0")'
#alias resizescreen='xrandr -s 1 && xrandr -s 0' #reset screen resolution to default

#https://superuser.com/questions/611538/is-there-a-way-to-display-a-countdown-or-stopwatch-timer-in-a-terminal
function countdown(){
   date1=$((`date +%s` + $1));
   while [ "$date1" -ne `date +%s` ]; do
     sleep 0.5
     echo -ne "$(date -u --date @$(($date1 - `date +%s`)) +%H:%M:%S)\r";
   done
}
function stopwatch(){
  date1=`date +%s`;
   while true; do
     sleep 0.5
    echo -ne "$(date -u --date @$((`date +%s` - $date1)) +%H:%M:%S)\r";
   done
}

#https://stackoverflow.com/questions/5566310/how-to-recursively-find-and-list-the-latest-modified-files-in-a-directory-with-s
function find_recently_changed() {
  find $1 -type f -print0 | xargs -0 stat --format '%Y :%y %n' | sort -nr | cut -d: -f2- | head
}

#VNC and nested X stuff
function xephyr(){
  Xephyr $1 -ac -dpi 96 -reset -screen $2 2>&1 >/dev/null &
  pid=$!
  DISPLAY=$1 $3 &
  sh -c "trap 'kill $pid; exit' INT; while [ 1 -lt 2 ] ; do sleep 5 ; done"
}

function vnckill() {
  vncserver -kill :1
}
function vncserve() {
  if [ $# -eq 0 ]; then GEO="1200x730"; else GEO="$1"; fi
  vnckill; vncserver -geometry $GEO -alwaysshared -localhost -SecurityTypes None :1
}

#Homework assignments
#--------------------
#Go to ranger bookmark
cdmark() {
  if [[ "$1" = "" ]]; then
    cat ~/.config/ranger/bookmarks
  else
    cd $(grep "^$1" ~/.config/ranger/bookmarks | sed "s/^$1://")
  fi
}

#Copy tex file into subdir
#requires to have a whatever_base.tex file with placeholder NUM
hwtex() {
  if [ $# -eq 0 ]  #no arguments
  then
    echo "No number given!"
    return 1
  fi

  num=$(printf "%02d" $1)
  match=$(find ./ -name "*_base.tex"|sort|tail -n 1)
  if [ -z "$match" ] #no file found
  then
    echo "No base file (ending with _base.tex) found!"
    return 1
  fi

  mkdir L$num
  cat $match | sed "s/NUM/$num/" > L$num/$(sed "s/base/$num/" <<< $match)
  cd L$num
}

#Recursive javac in root source directory (poor man's build system)
javacrec() {
  #Fix encoding of files to UTF-8
  find . -name "*.java" -exec sh -c 'iconv -f ISO-8859-15 -t UTF-8 "$1" > tmp; mv tmp "$1"' x {} \;
  #Compile
  find -name "*.java" > .java_sources.txt
  javac @.java_sources.txt
  rm -f .java_sources.txt
}

#Recompile pdf (shrinking, removing password) repdf in out [password]
repdf() {
  if [ -n "$3" ]; then
    PASSWORDARG="-sPDFPassword=$3"
  fi
  #add -dPDFSETTINGS=/screen for lower quality, ebook=middle, printer=good, prepress=best
  gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 -dAutoRotatePages=/None -dNOPAUSE -dQUIET -dBATCH $PASSWORDARG -sOutputFile=$2 $1
}
#--------------------

#say over google tts
say() { mplayer "http://translate.google.com/translate_tts?ie=UTF-8&tl=en&q=$1";}

#Resolve Uni Lübeck pc pool hostname to IP using ssh-gate
poolpc() { ssh sshgate ping -c 1 $1 | head -n 1 | awk '{print$3}' | sed 's/[()]//g';}

#Create socks proxy to uni lübeck intranet for usage with tsocks etc running on port 7070
uniconnect() { ssh -N -D7070 sshgate;}

#Reverse tunnel to $1:$2
reversetunnel() { autossh -M 0 -o "ServerAliveInterval 30" -o "ServerAliveCountMax 2" -N -R $2:localhost:2200 $1; }

# Cool History Summerizer - most used commands
top10cmds(){ history|awk '{a[$2]++}END{for(i in a){printf"%5d\t%s\n",a[i],i}}'|sort -nr|head;}

#Make sure ssh-agent is running with keychain before using git push
git() {
  if [ "push" == "$1" ] || [ "pull" == "$1" ]
  then
    eval `keychain --eval --nogui -Q -q id_rsa`
  fi
  $(which git) "$@"
}

#Handy batch imagemagick foto modification shortcut
#example: batchconv -resize 33%
batchconv() {
  mkdir modified
  find . -iname "*.jpg" -o -iname "*.png" | xargs -l -i convert "$@" {} ./modified/{}
}

shrinkimg() { convert -resize 33% "$1" "$1.jpg";}

#My vnc shortie: vnc host password other_options
vnc(){ echo $2|vncviewer -compresslevel 9 -quality 7 -autopass $3 $1; }

#Ruby calculator
calc(){ ruby -e "require 'mathn'; puts $1";}

#String escape method into hex, usage: escape STRING ESCPREFIX(like % or 0x)
escape(){ echo "puts '$2'+'$1'.split(//).map{|x| x.slice(0).ord.to_s(16)}.join('$2')"|ruby; }

#German-English translation... via dict.cc, requires curl, grep and ruby
translate(){ curl --silent http://www.dict.cc/?s=$1|grep c[12]Arr | ruby -e "arr=[gets.split('\",\"'),gets.split('\",\"')]; arr[0].shift; arr[1].shift; arr.each{|x| x[-1]=x[-1][0..-5] }; 0.upto(arr[0].length-1){|n| puts arr[0][n]+' <-> '+arr[1][n] }"; }

#set 256 color color
Set256Color(){ if [ "$TERM" != "linux" ];then echo -e "\e[38;5;$1m";fi;}

assignProxy(){
  PROXY_ENV="http_proxy ftp_proxy https_proxy all_proxy HTTP_PROXY HTTPS_PROXY FTP_PROXY ALL_PROXY"
  for envar in $PROXY_ENV
  do
    export $envar=$1
  done
}
clrProxy(){
  assignProxy "" # This is what 'unset' does.
}

#Dump transactions to file
getBankingCSV(){
  aqbanking-cli request --transactions -a "$1" | aqbanking-cli listtrans > "$2"
}

#GREETING (Distribution, Kernel Version, Date+Time, Uptime)
echo -e "\e[1;36m$(Set256Color 51)        ,                        _     _ _
       /#\         __ _ _ __ ___| |__ | (_)_ __  _   ___  __
$(Set256Color 45)      ,###\       / _\` | '__/ __| '_ \| | | '_ \| | | \ \/ /
\e[0;36m$(Set256Color 39)     /#####\     | (_| | | | (__| | | | | | | | | |_| |>  <
$(Set256Color 33)    /##,-,##\     \__,_|_|  \___|_| |_|_|_|_| |_|\__,_/_/\_\\
   /##(   )##\`   \e[0;33m\t$(uname -o) Kernel $(uname -r)
\e[0;36m$(Set256Color 27)  /#.--   --.#\  \e[0;32m$(date "+%a, %e. %B %Y %H:%M:%S"), uptime:$(uptime|head -c 18|tail -c 5)
\e[0;36m$(Set256Color 27) /\`           \`\ "

