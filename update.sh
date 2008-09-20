#!/bin/bash
case $1 in
pull)
	Message="Pulled latest data from local setup."
	cp $HOME/.emacs ./dot_emacs
	cp -r $HOME/.emacs.d ./emacs.d
	cp $HOME/.Xresources ./Xresources
	;;
install)
	Message="Installed from repo into home directory."
	cp ./dot_emacs $HOME/.emacs
	cp -r ./emacs.d $HOME/.emacs.d
	cp ./Xresources $HOME/.Xresources
	;;
*)
	Message="Unknown command (pull, install)"
	;;
esac

echo $Message

