YAOURT_FILE 	:= yaourt_backup.txt
PACMAN_FILE 	:= pacman_backup.txt

backup:
	@echo backing up ...
	pacman -Qqem > $(YAOURT_FILE)
	pacman -Qqen | grep -v "`cat yaourt_backup.txt`" > $(PACMAN_FILE)
	cp /etc/pacman.conf pacman.conf

restore:
	@echo restoring packages ...
	sudo pacman -S --needed `cat $(PACMAN_FILE)`
	cat $(YAOURT_FILE) | xargs yaourt -S --noconfirm --needed

pacman-init:
	sudo cp pacman.conf /etc/pacman.conf
	sudo pacman-key --init
	sudo pacman-key --populate archlinux

clean:
	@echo clean unused packages ...
	yaourt -Rsn $(yaourt -Qdtq)
