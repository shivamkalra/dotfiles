New System
==========
* Replace the `pacman.conf` with `/etc/pacman.conf`
* Initialize pacman keyring
* Run `pacman -S --needed $(cat package_list.txt)`
* To get community packages `yaourt -S $(cat /opt/restore/pkglist-loc.txt | grep -vx "$(pacman -Qqm)")`
* [More Example](https://wiki.archlinux.org/index.php/System_Restore_from_Configurations "System Restore from Configurations")