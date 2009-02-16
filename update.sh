#!/bin/bash
case $1 in
pull)
	Message="Pulled latest data from local setup."
	cp $HOME/.emacs ./dot_emacs
	mkdir -p ./emacs.d
	cp -r $HOME/.emacs.d/* ./emacs.d/
	cp $HOME/.Xresources ./Xresources
	cp $HOME/bin/epylint ./
	;;
dotemacs)
	Message="Installed .emacs from repo into home directory."
	cp ./dot_emacs $HOME/.emacs
	;;
colors)
	Message="Installed Tango Light from repo into home directory."
	cp ./emacs.d/color-theme/color-theme-tango-light.el ~/.emacs.d/color-theme/color-theme-tango-light.el
	;;
install)
	Message="Installed from repo into home directory."
	cp ./dot_emacs $HOME/.emacs
	mkdir -p $HOME/.emacs.d
	cp -r ./emacs.d/* $HOME/.emacs.d/
	cp ./Xresources $HOME/.Xresources
	mkdir $HOME/bin
	cp ./epylint $HOME/bin/
	;;
*)
	Message="Unknown command (pull, install)"
	;;
esac

echo $Message

