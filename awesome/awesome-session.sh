#!/bin/bash
export OOO_FORCE_DESKTOP=gnome
(sleep 1 && gnome-settings-daemon) &
(sleep 1 && gnome-power-manager) &
(sleep 1 && gnome-keyring-daemon) &
#gnome-screensaver &
(sleep 1 && gnome-volume-control-applet) &
(sleep 1 && bluetooth-applet) &
#xcompmgr -a &
(sleep 1 && nm-applet) &
#(sleep 4 && gnome-do)&
#nitrogen --restore &
#thunar --daemon &
#unclutter -idle 8 -root &
exec awesome

