#Anton Pirogov's .bashrc
[ -r /etc/profile ] && . /etc/profile

#Add own binaries
#Global cabal sandbox only for recent cabal-install and xmonad
paths=(~/bin /home/admin/bin/rpi/haskell/ghc-7.10.2/bin /home/admin/bin/rpi/gcc-linaro-4.8/bin ~/bin/matlab2015b/bin ~/bin/sandboxes/*/bin ~/.cabal/bin  ~/.gem/ruby/*/bin)
for bindir in ${paths[@]}; do [ -d $bindir ] && PATH=$PATH:${bindir}; done
#add non-system .so libs
libs=()
# for libdir in ${libs[@]}; do [ -d $libdir ] && LD_LIBRARY_PATH=$LD_LIBRARY_PATH:${libdir}; done
# export LD_LIBRARY_PATH

[ -z "$PS1" ] && return # If not running interactively, don't do anything

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
set -o vi           # vi input mode

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
#Btw: Alt-Backspace deletes word, CTRL-<Arrow> moves wordwise, !CMD runs last matching cmd

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
export LANGUAGE="de_DE.UTF-8"
export TZ="Europe/Berlin"

export HISTCONTROL="ignorespace:ignoredups:erasedups" #No duplicates in history, no cmds preceded by space
export HISTIGNORE="&:ls:ll:la:q:pwd:exit:clear"	#Never log these
export HISTSIZE="9999"	#Lines saved in one session
export HISTFILESIZE="999999"	#Lines total
export PROMPT_COMMAND+="history -a; history -n" #share history between terminals

export BROWSER="firefox"
export EDITOR="nvim"
export VISUAL="gvim"
export PAGER="less"
export LESS="-iMn -F -X -R"
export TEXMFHOME="~/.texmf"   #custom packages

## ALIASES AND FUNCTIONS
alias ls='ls --color=auto'
alias la='ls -A'
alias ll='ls -lhtA'
alias q='exit'
alias e='nvim'
alias rm='rm -iv'       #safety - ask before delete/overwrite
alias cp='cp -iv'
alias mv='mv -iv'
alias mkdir='mkdir -p -v' #Allow multiple levels at once
alias ps='ps -e -o pid,user,comm,args,vsize,pcpu' #Tweaked process list
alias df='df -hT'       #-h : Human readable
alias du='du -sh'
alias duf='du -sk * | sort -n | perl -ne '\''($s,$f)=split(m{\t});for (qw(K M G)) {if($s<1024) {printf("%.1f",$s);print "$_\t$f"; last};$s=$s/1024}'\' #readable + sorted
alias findtext="grep --color=always -ri" #better use ag when installed
alias findfile='find . -name'

alias ping='ping -c 5'	#Limit ping number
alias pingl='ping6 ff02::1%eth0'  #ping local devices

#Filesystem
alias tcrypt='sudo ~/.tcrypt.sh' #tcplay wrapper. Usage: tcrypt open|close CONTAINERFILE
alias mkisofs='mkisofs -v -r -J -o'	#Usage: mkisofs target.img /src/path
alias mirror="rsync -auv --delete"

#Compiler settings
alias hc='rm -rf /tmp/*.o; ghc -Wall -fwarn-name-shadowing -fwarn-incomplete-patterns -hidir=/tmp -odir=/tmp -O' #"script compile" shortie
alias gcc='LANG="C" gcc -ansi -std=c99 -pedantic -Wall -Wextra -Wshadow -Wcast-qual -Wformat=2 -Wmissing-include-dirs -Wfloat-equal -Wswitch-enum -Wundef -Wwrite-strings -Wredundant-decls -fverbose-asm -pg -g '	#High standard level, many debugging opts
#Recursive javac in root source directory, fixing UTF and setting up dirs
javacrec() {
  # echo "convert to unicode.."
  # find -name "*.java" -exec sh -c 'iconv -f ISO-8859-15 -t UTF-8 "$1" > tmp; mv tmp "$1"' x {} \;
  echo "create package structure.."
  dir="grep package \$1 | sed 's/[\\t ]*package[\\t ]*\\([a-zA-Z0-9.]*\\)[\\t ]*;.*/\\1/' | sed 's/\\./\\//'"
  find -name "*.java" -exec sh -c "dir=\$($dir); [ -n \"\$dir\" ] && mkdir -p \$dir && mv \$1 \$dir" x {} \;
  echo "compile!"
  find -name "*.java" > .java_sources.txt; javac -encoding iso-8859-1 @.java_sources.txt; rm -f .java_sources.txt
}
#Poor man's presentation generation, usage: genpres ~/*pics.jpg > out.pdf
#${@:1:$(($#-1))}
genpres() { convert -crop +1+1 -crop -1-1 -gravity center -extent 1600x1200 $@ pdf:-; }
#Shots to movie
genmovie() { ffmpeg -framerate 4 -r 4 -pattern_type glob -i "$1"  -c:v libx264 -vf "fps=25,format=yuv420p" $2; }

#Package Management - Pacman
if which pacman >/dev/null; then
  function pss(){ aura -Ss --color=always $@; aura -As $@; } #Search for packages
  function pip(){ sudo -- sh -c "pacman -S --needed $@ || aura -A $@"; } #Install package
  alias pup='sudo -- sh -c "aura -Syu; aura -Au"'   #Dist upgrade
  alias prp='sudo pacman -Ruscn' #Recursive remove
  alias pil='sudo pacman -U'     #Install local Package
  alias pro='sudo aura -Oj'      #List and remove orphans
  alias pgl='pacman -Qqne'       #List of installed packages
  alias pgf='pacman -Qqme'       #List foreign/AUR packages
  alias pql='pacman -Ql'         #List files installed by that package
fi
if which systemctl >/dev/null; then
  alias listd="systemctl list-unit-files --type=service" #show all daemons run on startup
fi

#SSH / Remote stuff
alias tmux='tmux -2' #note: copy/paste with mouse holding Shift within tmux
alias initsshkeys=''
if which keychain >/dev/null; then #SSH key management
  alias initsshkeys='eval `keychain --eval --nogui -Q -q id_rsa 2>/dev/null`'
  git() { #Make sure ssh-agent is running with keychain before using git push
    if [ "push" == "$1" ] || [ "pull" == "$1" ]; then
      eval `keychain --eval --nogui -Q -q id_rsa 2>/dev/null`
    fi
    $(which git) "$@"
  }
fi
alias ssh='initsshkeys && ssh'
alias sshfs='initsshkeys && sshfs -o idmap=user'
reversetunnel() { autossh -M 0 -o "ServerAliveInterval 30" -o "ServerAliveCountMax 2" -N -R $2:localhost:2200 $1; } #Reverse tunnel to $1:$2
sshproxy() { ssh -N -D7070 $1; } #Create socks proxy for usage with e.g. tsocks running on port 7070
sshtmux(){ ssh -t $1 tmux -2 new -A -s main; }
sshscreen(){ ssh -t $1 screen -R; }

#VNC
function vnckill() { vncserver -kill :1; }
function vncserve() {
  if [ $# -eq 0 ]; then GEO="1200x730"; else GEO="$1"; fi
  vnckill; vncserver -geometry $GEO -alwaysshared -localhost -SecurityTypes None :1
}
#Run vncserver on remote host and create port forwarding for one minute
sshvncup(){
  vncopts="-alwaysshared -localhost -autokill -SecurityTypes None :1"
  ssh -f -L 5901:localhost:5901 $1 "vncserver $vncopts && sleep 60"
}
sshvncdown(){ ssh $1 "vncserver -kill :1"; }

#Nested X Server (xephyr display size exec)
function xephyr(){
  Xephyr $1 -ac -dpi 96 -reset -screen $2 2>&1 >/dev/null &
  pid=$!
  DISPLAY=$1 $3 &
  sh -c "trap 'kill $pid; exit' INT; while [ 1 -lt 2 ] ; do sleep 5 ; done"
}


#Internet CLI
alias getip="wget -O - -q http://checkip.dyndns.org/index.html|sed -e 's/.* //' -e 's/<.*//'"
alias speedtest='wget -O /dev/null http://speedtest.wdc01.softlayer.com/downloads/test10.zip'
say() { mpv "http://translate.google.com/translate_tts?ie=UTF-8&tl=en&q=$(escape "$1" %)";} #say over google tts
#German-English translation... via dict.cc, requires curl, grep and ruby
translate(){ curl --silent http://www.dict.cc/?s=$1|grep c[12]Arr | ruby -e "arr=[gets.split('\",\"'),gets.split('\",\"')]; arr[0].shift; arr[1].shift; arr.each{|x| x[-1]=x[-1][0..-5] }; 0.upto(arr[0].length-1){|n| puts arr[0][n]+' <-> '+arr[1][n] }"; }
getunicode(){ wget -O - http://www.fileformat.info$(wget -O - http://www.fileformat.info/info/unicode/char/$1/index.htm 2>/dev/null | grep $1 | grep img | sed 's/.*src="//' | sed 's/".*//') 2>/dev/null | display; }

# Proxy
assignProxy(){
  PROXY_ENV="http_proxy ftp_proxy https_proxy all_proxy HTTP_PROXY HTTPS_PROXY FTP_PROXY ALL_PROXY"
  for envar in $PROXY_ENV; do export $envar=$1; done
}
clrProxy(){ assignProxy ""; } # This is what 'unset' does.

#Misc. Programs
alias togglepad='killall syndaemon; synclient TouchpadOff=$(synclient -l | grep -c "TouchpadOff.*=.*0")'
alias togglelrm="xmodmap -pp | grep -e '\\s\+[13]\\s' | awk '{print \$2}' | sed 'N;s/\n/ /' | xargs -l bash -c 'xmodmap -e \"pointer = \$1 2 \$0\"'"
alias resetscreen='xrandr -s 1 && xrandr -s 0' #reset screen resolution to default (after buggy games)
alias xp='xprop | grep "WM_WINDOW_ROLE\|WM_CLASS" && echo "WM_CLASS(STRING) = \"NAME\", \"CLASS\"";xwininfo'
alias top10size='find . -printf "%s %p\n"|sort -nr|head'
matlabrepl(){ tmux -2 new -A -s mat "matlab -nosplash -nodesktop $@"; }
top10cmds(){ history|awk '{a[$2]++}END{for(i in a){printf"%5d\t%s\n",a[i],i}}'|sort -nr|head; } # most used commands
find_recently_changed(){ find $1 -type f -print0 | xargs -0 stat --format '%Y :%y %n' | sort -nr | cut -d: -f2- | head; }
batchconv(){ mkdir modified; find . -iname "*.jpg" -o -iname "*.png" | xargs -l -i convert "$@" {} ./modified/{}; } #example: batchconv -resize 33%
shrinkimg(){ convert -resize 33% "$1" "$1.jpg"; }
pulsekill(){ echo autospawn = no > $HOME/.config/pulse/client.conf; pulseaudio --kill; rm $HOME/.config/pulse/client.conf; } #disable pulse
getBankingCSV(){ aqbanking-cli request --transactions -a "$1" | aqbanking-cli listtrans > "$2"; } #Dump transactions to file
escape(){ ruby -e "puts '$2'+'$1'.split(//).map{|x| x.slice(0).ord.to_s(16)}.join('$2')"; } #usage: escape STRING ESCPREFIX(like % or 0x)
calc(){ ruby -e "require 'mathn'; puts $1"; } #Ruby calculator

if which ghc >/dev/null; then #define these if ghc is present
  function hcalc { ghc -e "($*)"; }
  function hmap { ghc -e "interact ($*)"; }
  function hmapl { hmap "unlines.($*).lines"; }
  function hmapw { hmapl "map (unwords.($*).words)"; }

  alias ghc-sandbox="ghc -no-user-package-db -package-db .cabal-sandbox/*-packages.conf.d"
  alias ghci-sandbox="ghci -no-user-package-db -package-db .cabal-sandbox/*-packages.conf.d"
  alias runhaskell-sandbox="runhaskell -no-user-package-db -package-db .cabal-sandbox/*-packages.conf.d"
fi

#Set Urxvt font size on the fly
fontsize() {
  font=$(xrdb -query | grep URxvt.*font | sed 's/.*:[\t ][\t ]*\(.*\)/\1/')
  newsize=$(echo $font | sed "s/\\(.*size=\\)\\([0-9]*\\)\\(.*\\)/\\1$1\\3/")
  echo -e "\033]710;$newsize\033\\"
}

repdf() { #Recompile pdf (shrinking, removing password). Usage: repdf in out [password]
  if [ -n "$3" ]; then PASSWORDARG="-sPDFPassword=$3"; fi
  #add -dPDFSETTINGS=/screen for lower quality, ebook=middle, printer=good, prepress=best
  gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 -dAutoRotatePages=/None -dNOPAUSE -dQUIET -dBATCH $PASSWORDARG -sOutputFile=$2 $1
}
alias joinpdf='gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=join.pdf ' #requires ghostscript, convert also works!
alias cleantex='find . -regex ".*\(aux\|bbl\|blg\|brf|\idx\|ilg\|ind\|lof\|log\|lol\|lot\|out\|nav\|out\|snm\|tdo\|toc\|fls\|fdb_latexmk\)$" -exec rm -i {} \;'

#Music
alias setmp3chmod='find -name "*.mp3" -print0 | xargs -0 chmod 644'
alias fixmusicdir='chmod -R u+rwX,go+rX,go-w ./'  #set files to 644, dirs to 755
alias m4a2mp3='for a in *.m4a; do faad -f 2 -w "$a"  | lame -r - "$a.mp3"; done'
alias normalizevolume='find /home/admin/myfiles/music/ -type f -iname "*.mp3" -exec mp3gain -p -r -k -s i -d 6.0 "{}" \;'

#----
#mediacenter - using ssh/sshfs/rsync/mpd/ncmpcpp
#use to: backup folders, mount remote data, control music
#requires to have MPD default port, MEDIASSHP for ssh and port 8000 for mpd streaming open on media server
MEDIAUSER=admin       #user for ssh connection
MPDPWDFILE=~/.mpdpwd  #.mpdpwd, which should contain PWD@HOST per line
MEDIASSHP=2200        #SSH port
MEDIAHOST=$(head -n 1 $MPDPWDFILE | sed 's/.*@//') #takes first host from list for following aliases
MEDIALOGIN=$MEDIAUSER@$MEDIAHOST
alias center.mount="mkdir ~/media; sshfs -p $MEDIASSHP $MEDIALOGIN:/media/DATA ~/media"
alias center.umount="fusermount -u ~/media; rmdir ~/media"
alias center.updatebackup="rsync --info=progress2 --delete -avue 'ssh -p $MEDIASSHP' ~/myfiles $MEDIALOGIN:/media/DATA" #mirror local -> remote external
alias center.mpc="ncmpcpp -h $MEDIAHOST"
alias center.stream="mpv http://$MEDIAHOST:8000"
#alias to access either first mpd from .mpdpwd -> mediaserver, or fallback to localhost
SMARTMPD='$(if ping -c 1 -w 1 $MEDIAHOST > /dev/null; then echo $(head -n 1 $MPDPWDFILE); else echo localhost; fi)'
alias music="ncmpcpp -h $SMARTMPD"
#----

#https://superuser.com/questions/611538/is-there-a-way-to-display-a-countdown-or-stopwatch-timer-in-a-terminal
function countdown(){ date1=$((`date +%s` + $1)); while [ "$date1" -ne `date +%s` ]; do
  sleep 0.5; echo -ne "$(date -u --date @$(($date1 - `date +%s`)) +%H:%M:%S)\r"; done; }
function stopwatch(){ date1=`date +%s`; while true; do
  sleep 0.5; echo -ne "$(date -u --date @$((`date +%s` - $date1)) +%H:%M:%S)\r"; done; }

# wraps cd, autocompletion removes one dir for each <TAB>-press, cycling
# To move forward again, append a / and <TAB>
function ..() { cd $1; }
function _..() {
  local cur=${COMP_WORDS[COMP_CWORD]}
  local lastchr=${cur: -1}
  local esc='s/ /\\ /g'
  if [[ "$cur" = "/" ]]; then
    COMPREPLY=("$(pwd | sed "$esc")")
  elif [[ "$cur" = "" ]]; then
    COMPREPLY=("$(cd ..; echo $(pwd | sed "$esc"))")
  elif [[ "$lastchr" = "/" ]]; then
    COMPREPLY=("; cd $(echo $cur)")
  else
    local unescur=$(echo $cur | sed 's/\\ / /g')
    if [[ -d "$unescur" ]]; then
      COMPREPLY=("$(cd "$unescur/.."; echo $(pwd | sed "$esc"))")
    else
      COMPREPLY=()
    fi
  fi
}
complete -o nospace -F _.. ..

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

#Homework assignments
#--------------------
#Copy tex file into subdir - basically mini tex template automation
#requires to have a whatever_base.tex file with placeholder NUM in current directory
hwtex() {
  if [ $# -eq 0 ]; then echo "No number given!"; return 1; fi
  num=$(printf "%02d" $1)
  exists=0
  if [ -d "L$num" ]; then echo "Opening existing..."; exists=1; fi
  match=$(find ./ -name "*_base.tex"|sort|tail -n 1)
  if [ -z "$match" ]; then echo "No base file (ending with _base.tex) found!"; return 1; fi
  file=$(sed "s/base/$num/" <<< $match)
  if [ $exists -eq 0 ]; then
    mkdir L$num; cat $match | sed "s/NUM/$num/" > L$num/$file
  fi
  cd L$num
  vim $file +Latexmk +LatexView
}
#get free pool computer
poolfree() {
  for i in {1..70}; do
    host=64pc$(printf %02d $i)
    resp=$(ssh $host users)
    if [[ -z "$resp" ]]; then
      echo $host
      return
    fi
  done
}
#Push to Pool to Print
ppp() {
  host=$(poolfree)
  list() { echo "PDFs in ~:"; ssh $host 'ls *.pdf'; unset -f list; }
  initsshkeys
  if [[ "$1" == "" ]]; then list; return; fi
  if [[ "$(ssh $host "md5sum $1 2>/dev/null")" == "$(md5sum $1)" ]]; then
    echo "Already up to date!"; list; return; fi
  scp $1 sshgate:~; sleep 1; echo -n "MD5: "
  if [[ "$(ssh $host "md5sum $1 2>/dev/null")" == "$(md5sum $1)" ]]; then
    echo "Success!"; else echo "Failure!"; fi
  list
}
complete -f -X '!*.@(pdf)' ppp #complete only pdf files

socksend() {
  exec 3<>/dev/tcp/$1/$2
  echo $3 >&3
  exec 3>&-
}
