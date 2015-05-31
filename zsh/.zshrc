# Print some cows
cowsay "Hello Cow" 2> /dev/null

# add path
export PATH="/usr/bin":${HOME}/bin:${HOME}/.cask/bin/:${HOME}/.cabal/bin:${HOME}/go/bin:$PATH

#Python Environment
export WORKON_HOME=$HOME/.virtualenvs
source /usr/bin/virtualenvwrapper_lazy.sh

# load zgen
source "${ZDOTDIR}/zgen/zgen.zsh"

# settings for zsh auto-completion
export AUTOSUGGESTION_HIGHLIGHT_COLOR="fg=6"
export AUTOSUGGESTION_ACCEPT_RIGHT_ARROW=1

# check if there's no init script
if ! zgen saved; then
    echo "Creating a zgen save"

    # Load robbyrussell's oh-my-zsh's library
    zgen oh-my-zsh

    # Plugins from robbyrussell's oh-my-zsh
    zgen oh-my-zsh plugins/tmux
    zgen oh-my-zsh plugins/git
    zgen oh-my-zsh plugins/pip
    zgen oh-my-zsh plugins/python
    zgen oh-my-zsh plugins/extract
    zgen oh-my-zsh plugins/virtualenv
    zgen oh-my-zsh plugins/command-not-found
    zgen oh-my-zsh plugins/virtualenvwrapper

    # Github plugins
    zgen zsh-users/history-substring-search﻿
    zgen load rupa/z
    zgen load jimmijj/zsh-syntax-highlighting
    zgen load tarruda/zsh-autosuggestions
    zgen load kennethreitz/autoenv
    zgen load tarruda/zsh-autosuggestions

    # Load theme
    zgen oh-my-zsh themes/ys

    # Tell antigen that you're done
    zgen save

fi

# Enable autosuggestions automatically.
zle-line-init() {
    zle autosuggest-start
}
zle -N zle-line-init

# ls colors
eval `dircolors ~/.lscolors`

################################################################
# Aliases                                                      #
################################################################
alias l="ls"
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
