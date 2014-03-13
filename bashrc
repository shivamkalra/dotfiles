################################################################
# Functions                                                    #
################################################################
extract () {
   if [ -f $1 ] ; then
       case $1 in
           *.tar.bz2)   tar xvjf $1    ;;
           *.tar.gz)    tar xvzf $1    ;;
           *.bz2)       bunzip2 $1     ;;
           *.rar)       unrar x $1       ;;
           *.gz)        gunzip $1      ;;
           *.tar)       tar xvf $1     ;;
           *.tbz2)      tar xvjf $1    ;;
           *.tgz)       tar xvzf $1    ;;
           *.zip)       unzip $1       ;;
           *.Z)         uncompress $1  ;;
           *.7z)        7z x $1        ;;
           *)           echo "don't know how to extract '$1'..." ;;
       esac
   else
       echo "'$1' is not a valid file!"
   fi
 }

################################################################
# Variables                                                    #
################################################################
export EDITOR=emacs
export TERMINAL=urxvt
export BROWSER=chromium
export PS1=""
################################################################
# Aliases                                                      #
################################################################
alias emacs="TERM=rxvt emacs -nw"
alias ls='ls -h --color=auto'
alias h='cd'
alias ..='cd ..'
alias cd..='cd ..'
alias cim='vim'
alias back='cd $OLDPWD'
alias root='sudo su'
alias grep='grep --color=auto'
alias dfh='df -h'
alias gvim='gvim -geom 84x26'
alias start='dbus-launch startx'

CLEAR="\e[0m"
RED="\e[31m"
GREEN="\e[32m"
YELLOW="\e[33m"
BLUE="\e[34m"

export PS1="\[$RED\]>>\[$GREEN\]>>\[$BLUE\]>>\[$CLEAR\] "
# Gtk themes
export GTK2_RC_FILES="$HOME/.gtkrc-2.0"

# [ ! "$UID" = "0" ] && archbey2
eval `dircolors ~/.lscolors`
