minimal: install-emacs install-bash install-git \
         install-xorg

install: install-emacs install-bash install-git \
         install-xorg install-mpd \
         install-ncmpcpp install-tmux \
	 install-bin install-awesome

install-emacs:
	rm -rf ~/.emacs.d
	ln -s `pwd`/emacs ~/.emacs.d

install-bash:
	rm -f ~/.bashrc ~/.bash_profile
	ln -s `pwd`/bash/bashrc ~/.bashrc
	ln -s `pwd`/bash/bash_profile ~/.bash_profile

install-git:
	rm -f ~/.git-completion.bash
	ln -s `pwd`/git/git-completion.bash ~/.git-completion.bash

install-xorg:
	mv -f ~/.xinitrc ~/.xinitrc.bak 2> /dev/null; true
	rm -f ~/.Xdefaults ~/.lscolors
	ln -s `pwd`/xorg/xinitrc ~/.xinitrc
	ln -s `pwd`/xorg/Xdefaults ~/.Xdefaults
	ln -s `pwd`/xorg/dircolors-solarized/dircolors.ansi-dark ~/.lscolors

install-mpd:
	rm -rf ~/.config/mpd
	ln -s `pwd`/mpd/ ~/.config/mpd

install-ncmpcpp:
	rm -rf ~/.ncmpcpp
	ln -s `pwd`/ncmpcpp ~/.ncmpcpp

install-tmux:
	rm -f ~/.tmux.conf
	ln -s `pwd`/tmux/tmux.conf ~/.tmux.conf

install-bin:
	rm -rf ~/bin
	ln -s `pwd`/bin ~/bin

install-awesome:
	rm -rf ~/.config/awesome
	ln -s `pwd`/awesome ~/.config/awesome
