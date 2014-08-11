#!/bin/bash

export DE=xfce
export XDG_CURRENT_DESKTOP=xfce

# DBUS fix
if [ -z "$DBUS_SESSION_BUS_ADDRESS" ]; then
    eval "$(dbus-launch --sh-syntax --exit-with-session)"
fi

xbindkeys &

/usr/bin/xdg-user-dirs-update

# start openbox
#/usr/bin/openbox-session > ~/.openbox_startup.log 2>&1

# start the music player daemon
/usr/bin/mpd
/usr/bin/start-pulseaudio-x11

# start wm now!
/usr/bin/awesome
