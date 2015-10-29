# Created by newuser for 5.1.1
eval `dircolors ~/.lscolors`

function zathura-tabbed {
    wid=`wmctrl -lx | grep "tabbed.tabbed" | cut -d' ' -f1`
    if [[ -z "${wid// }" ]]; then
        wid=$(tabbed -d);
    fi
    echo "$wid"
    /bin/zathura -e "$wid" "$@"
}

export WORKON_HOME=~/.virtualenvs
source /usr/bin/virtualenvwrapper.sh

export GOPATH=${HOME}/.gopath
export PATH=${HOME}/.local/bin:${HOME}/.cabal/bin:${HOME}/.gopath/bin:${HOME}/bin:$PATH
eval "$(fasd --init auto)"

# settings for zsh auto-completion
export AUTOSUGGESTION_HIGHLIGHT_COLOR="fg=10"
export AUTOSUGGESTION_ACCEPT_RIGHT_ARROW=1
BULLETTRAIN_VIRTUALENV_PREFIX="!"

. /etc/profile.d/fzf.zsh

source ${HOME}/.antigen.zsh

antigen-use oh-my-zsh

# define the plugins
antigen bundle zsh-users/zsh-completions src
antigen-bundle Peeja/ctrl-zsh
antigen-bundle fasd
antigen-bundle systemd
antigen-bundle ssh-agent
antigen-bundle extract
antigen-bundle zsh-users/zsh-syntax-highlighting
antigen-bundle zsh-users/zsh-history-substring-search
antigen bundle mafredri/zsh-async
antigen theme https://github.com/caiogondim/bullet-train-oh-my-zsh-theme bullet-train

antigen apply

# bind UP and DOWN arrow keys
zmodload zsh/terminfo
bindkey "$terminfo[kcuu1]" history-substring-search-up
bindkey "$terminfo[kcud1]" history-substring-search-down

# bind UP and DOWN arrow keys (compatibility fallback
# for Ubuntu 12.04, Fedora 21, and MacOSX 10.9 users)
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down

# bind P and N for EMACS mode
bindkey -M emacs '^P' history-substring-search-up
bindkey -M emacs '^N' history-substring-search-down

################################################################
# Aliases                                                      #
################################################################
alias l="ls"
alias powerperf="sudo cpupower frequency-set -g performance"
alias powersave="sudo cpupower frequency-set -g powersave"
alias config="cd ${XDG_CONFIG_HOME}"
alias damn='$(thefuck $(fc -ln -1))'
alias e="exit"
alias ls='ls -h --color=auto'
alias back='cd $OLDPWD'
alias grep='grep --color=auto'
alias c="clear"
alias rm="rm -i"
alias rmf="rm -f"
alias p="cd ${HOME}/projects/"
alias b="cd ${HOME}/books/"
alias r='cd "$(git rev-parse --show-toplevel)"'
alias moff="xrandr --output VGA1 --off; echo 'awesome.restart()' | awesome-client;"
alias mon="xrandr --output VGA1 --left-of LVDS1 --mode 1920x1080; sleep 1s && echo 'awesome.restart()' | awesome-client;"
