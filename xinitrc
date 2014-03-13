#!/bin/bash

export DE=openbox

# Dbus fix
if [ -z "$DBUS_SESSION_BUS_ADDRESS" ]; then
    eval "$(dbus-launch --sh-syntax --exit-with-session)"
fi

xbindkeys &

/usr/bin/xdg-user-dirs-update

# start openbox
#/usr/bin/openbox-session > ~/.openbox_startup.log 2>&1

/usr/bin/awesome
