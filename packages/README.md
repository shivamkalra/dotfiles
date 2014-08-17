New System
==========
* Replace the `pacman.conf` with `/etc/pacman.conf`
* Initialize pacman keyring
* Run `for x in $(cat package_list.txt); do pacman -S $x; done`