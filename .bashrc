#Anton Pirogov's .bashrc

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

#Completion
if [ -f /etc/bash_completion ]; then
      . /etc/bash_completion # Source completion code if available
fi

# If exists, add ~/bin to $PATH.
if [ -d ~/bin ] ; then
 PATH=~/bin:$PATH
fi
if [ -d ~/.cabal/bin ] ; then
 PATH=~/.cabal/bin:$PATH
fi

# X Terminal titles
export PROMPT_COMMAND=""
case "$TERM" in
 xterm*|rxvt*)
  PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD/$HOME/~}\007";'
  ;;
 *)
  ;;
esac

#Set Chars to WINDOWS-Keys on keyboard
#WARNING: These keys sometimes won't work as expected by some programs!
#if [ -n "$DISPLAY" ]; then
#xmodmap -e 'keycode 133 = dollar';
#xmodmap -e 'keycode 134 = at';
#fi

#Remap context key (no context menus anymore with this key!)
#Now you can use it as compose key to compose stuff like áêìöú
#or with shift+context as extended key
#xmodmap -e 'keycode 135 = Multi_key'	#As Compose key ala compose+^+a=â
#xmodmap -e 'keycode 135 = Mode_switch'	#As Mode switch (kinda 2nd AltGr)
#xmodmap syntax: xmodmap -e 'keycode NUM = normal, shift, ModeSwitch, MSw.+Shift, AltGr, AltGr+Shift

## SHOPT OPTIONS
shopt -s cdspell	# This will correct minor spelling errors in a cd command.
shopt -s histappend	# Append to history rather than overwrite
shopt -s checkwinsize	# Check window after each command
shopt -s dotglob	# files beginning with . to be returned in the results of path-name expansion.

## SET OPTIONS
#set -o vi		# Vi-like command entry mode
set -o noclobber	# prevent overwriting files with cat
set -o ignoreeof	# stops ctrl+d from logging me out
bind 'set completion-ignore-case on'
bind 'set show-all-if-ambiguous on'

#XTerm Escape Sequences for text color + attributes
#Change color: \e[Nm
#For Use in PS1 put this into \[ and \] otherwise there will
#be strange side effects! so: \[e[Nm\]
#
#values for N:
#
#0 default	1 bold	4 underline	5 blink	7 inverse	8 invisible
#22 unbold	24 not underline	25 not blink	27 not inverse	28 visible
#
#Default 16 system colors:
#FG BG (Foreground/Background)
#30	40	Black
#31	41	Red
#32	42	Green
#33	43	Yellow
#34	44	Blue
#35	45	Magneta
#36	46	Cyan
#37	47	White
#Hint: Bold+Color = brighter color(for FG), so 16 colors :)
#Alternative: instead of 3n 9n for FG and instead of 4n 10n for BG
#Hint: You can combine the above numbers in one escape sequence
#  separating the numbers with semicolons, e.g. \e[1;34m for bold blue FG
#Hint: If you want the REAL ESCAPE SEQUENCE, press CTRL+V,ESC
#(you use \e or \033 which has to be transformed with echo -e)
#
#256 Color mode (if supported) these special combinations:
#FG color to index X: 38;5;X
#BG color to index X: 48;5;X
#X = number 0 to 255

#PS1
#Default:
#PS1='\u@\h:\W\$ ' #Boring :-P

#PS1:
DEFAULT="\[\e[0m\]"
RED="\[\e[1;31m\]"
GREEN="\[\e[32m\]"
BLUE="\[\e[1;34m\]"
CYAN="\[\e[1;36m\]"
WHITE="\[\e[1;37m\]"
BLUEBG="\[\e[44m\]"
export PS1="$BLUEBG$WHITE[\t]$DEFAULT$WHITE>$BLUE\u$RED@$CYAN\h$DEFAULT:\W$GREEN\$$DEFAULT "

#EXPORTS
export LANG="de_DE.UTF-8"
export LC_ALL="de_DE.UTF-8"
export TZ="Europe/Berlin"
export HISTCONTROL="erasedups" #No duplicates at all
#export HISTCONTROL="ignoreboth" #No repeated cmds or cmds preceded by a space
export HISTIGNORE="&:ls:ll:la:pwd:exit:clear"	#Never log these
export HISTSIZE="9999"	#Lines saved in one session
export HISTFILESIZE="999999"	#Lines total
#export HISTTIMEFORMAT='%F %T '
export PROMPT_COMMAND+="history -a; history -n"
export BROWSER="firefox"
export EDITOR="vim"
export VISUAL="gvim"
export PAGER="less"
export LESS="-iMn -F -X -R"
export SHELL="bash"
#export TERM="xterm"
export OOO_FORCE_DESKTOP=gnome
export DISPLAY=":0.0"
export JAVA_HOME=/usr #OpenJDK

#ALIASES
#navigation
alias ls='ls --color=auto'
alias la='ls -A'
alias ll='ls -lhtA'

#file operation/standard tools
alias df='df -hT'		#-h : Human readable
alias du='du -sh'
alias duf='du -sk * | sort -n | perl -ne '\''($s,$f)=split(m{\t});for (qw(K M G)) {if($s<1024) {printf("%.1f",$s);print "$_\t$f"; last};$s=$s/1024}'\' #readable + sorted
alias rm='rm -iv'		#safety - ask before delete/overwrite
alias cp='cp -iv'
alias mv='mv -iv'
alias c='clear'			#Shorties
alias q='exit'
alias cleanhistory='tac ~/.bash_history | awk '!seen[$0]++' | tac > newhist'
alias rmbackup='rm -f *~'
alias mkdir='mkdir -p -v'	#Allow multiple levels at once
alias sym='ln -s'		#Symlink
alias top10size='find . -printf "%s %p\n"|sort -nr|head'
alias resizescreen='xrandr -s 1 && xrandr -s 0'

#Mounting my Player with mtpfs
alias mountplayer='mkdir player; sudo mtpfs -o allow_other player'
alias umountplayer='sudo umount player; rmdir player'


#Package Management - Pacman
alias ygl="pacman -Qe | less"	#Get List w. names of all inst. pkgs.
alias ysi='pacman -Si'				#Search Info about packages
alias yss='yaourt -Ss'				#Search for packages
alias yip='sudo pacman -U'		#Install local Package
alias yin='sudo pacman -S --needed'	#Install package from database
alias yrm='sudo pacman -Ruscn'		#Recursive remove
alias yup='yaourt -Syu --aur'		#Dist upgrade

#Misc. Programs
alias gcc='LANG="C" gcc -ansi -std=c89 -pedantic -Wall -Wextra -Wshadow -Wcast-qual -Wformat=2 -Wmissing-include-dirs -Wfloat-equal -Wswitch-enum -Wundef -Wwrite-strings -Wredundant-decls -fverbose-asm -pg -g '	#High standard level, many debugging opts
alias hc='rm -rf /tmp/*.o; ghc -fwarn-name-shadowing -hidir=/tmp -odir=/tmp -O' #"script compile" shortie
alias ping='ping -c 5'	#Limit ping number
alias ps='ps -e -o pid,comm,args,vsize,pcpu' #Tweaked process list
alias mkisofs='mkisofs -v -r -J -o'	#Usage: mkisofs target.img /src/path
alias xp='xprop | grep "WM_WINDOW_ROLE\|WM_CLASS" && echo "WM_CLASS(STRING) = \"NAME\", \"CLASS\"";xwininfo'
alias setmp3chmod='find -name "*.mp3" -print0 | xargs -0 chmod 644'
alias normalizevolume='find /media/DATA/myfiles/music/ -type f -iname "*.mp3" -print0 | xargs -0 mp3gain -r -k -s i -d 4'

#Hardware control
alias cdo='eject sr0'	#CD Open
alias cdc='eject -t sr0' 	#CD Close
alias showswap='cat /proc/swaps'
alias wakemompc='wol 00:0a:e6:fa:72:54'	#Wake-on-LAN mamas pc

alias mc="LD_LIBRARY_PATH='/opt/java/jre/lib/i386' java -jar Minecraft/minecraft.jar"

#FUNCTIONS
up() { for updirs in $(seq ${1:-1}); do cd ..; done; } #Move x dirs up

# Cool History Summerizer - most used commands
top10cmds(){ history|awk '{a[$2]++}END{for(i in a){printf"%5d\t%s\n",a[i],i}}'|sort -nr|head;}

#Handy Daemons Commands
start() { for arg in $*; do sudo /etc/rc.d/$arg start; done }
stop() { for arg in $*; do sudo /etc/rc.d/$arg stop; done }
restart() { for arg in $*; do sudo /etc/rc.d/$arg restart; done }
reload() { for arg in $*; do sudo /etc/rc.d/$arg reload; done }

#OWN FUNCTIONS
#Make sure ssh-agent is running with keychain before using git push
git() {
  if [ "push" == "$1" ] || [ "pull" == "$1" ]
  then
    eval `keychain --eval --nogui -Q -q id_rsa`
  fi
  $(which git) "$@"
}

#My vnc shortie: vnc host password other_options
vnc(){ echo $2|vncviewer -compresslevel 9 -quality 7 -autopass $3 $1; }

#Ruby calculator
calc(){ ruby -e "require 'mathn'; puts $1";}

#Regex killall
killallr() {
	sh -c "ps -e -o comm" | grep -e ^.*$1.*$ | grep -v grep | xargs killall
}

#ROT18
rot18(){ echo "puts'$1'.split(//).map{|x|x.slice 0}.map{|x|x>64&&x<91||x>96&&x<123?(((x-(x<93?65:97)+13)%26+(x<93?65:97)).chr):x<58&&x>47?(((x-43)%10+48).chr):x.chr}.join"|ruby; }

#String escape method into hex, usage: escape STRING ESCPREFIX(like % or 0x)
escape(){ echo "puts '$2'+'$1'.split(//).map{|x| x.slice(0).ord.to_s(16)}.join('$2')"|ruby; }

#Show extern IP
getip(){ wget -O - -q http://checkip.dyndns.org/index.html|sed -e 's/.* //' -e 's/<.*//';}

#German-English translation... via dict.cc, requires curl, grep and ruby
translate(){ curl --silent http://www.dict.cc/?s=$1|grep c[12]Arr | ruby -e "arr=[gets.split('\",\"'),gets.split('\",\"')]; arr[0].shift; arr[1].shift; arr.each{|x| x[-1]=x[-1][0..-5] }; 0.upto(arr[0].length-1){|n| puts arr[0][n]+' <-> '+arr[1][n] }"; }

unixtime(){ date +"%s"; }	#Just for fun

#set 256 color color
Set256Color(){ if [ "$TERM" != "linux" ];then echo -e "\e[38;5;$1m";fi;}

#GREETING (Distribution, Kernel Version, Date+Time, Uptime, Todo-List)
echo -e "\e[1;36m$(Set256Color 51)        ,                        _     _ _                  
       /#\         __ _ _ __ ___| |__ | (_)_ __  _   ___  __
$(Set256Color 45)      ,###\       / _\` | '__/ __| '_ \| | | '_ \| | | \ \/ /
\e[0;36m$(Set256Color 39)     /#####\     | (_| | | | (__| | | | | | | | | |_| |>  < 
$(Set256Color 33)    /##,-,##\     \__,_|_|  \___|_| |_|_|_|_| |_|\__,_/_/\_\\
   /##(   )##\`   \e[0;33m\t$(uname -o) Kernel $(uname -r)
\e[0;36m$(Set256Color 27)  /#.--   --.#\  \e[0;32m$(date "+%a, %e. %B %Y %H:%M:%S"), uptime:$(uptime|head -c 18|tail -c 5)
\e[0;36m$(Set256Color 27) /\`           \`\ "

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
#setxkbmap de neo -option	#set neo keyboard layout
#set Scroll-lock key to switch QWERTZ (default) and NEO
setxkbmap -layout de,de -variant nodeadkeys,neo -option -option grp:sclk_toggle -option grp_led:scroll
#set Scroll-lock key to switch NEO (default) and QUERTZ
#setxkbmap -layout de,de -variant neo,nodeadkeys -option -option grp:sclk_toggle -option grp_led:scroll

#activate wake-on-lan
sudo ethtool -s eth0 wol g  #enable wake on lan
#activate webcam mic
sudo modprobe snd-usb-audio 1>&2 2>/dev/null
