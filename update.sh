#!/bin/bash
case $1 in
pull)
	Message="Pulled latest data from local setup."
	cp $HOME/.emacs ./dot_emacs
	if [ ! -d ./emacs.d ] ; then
		mkdir ./emacs.d
	fi
	cp -r $HOME/.emacs.d/* ./emacs.d/
	cp $HOME/.Xresources ./Xresources
	cp $HOME/bin/epylint ./
	;;
install)
	Message="Installed from repo into home directory."
	cp ./dot_emacs $HOME/.emacs
	if [ ! -d $HOME/.emacs.d ] ; then
		mkdir $HOME/.emacs.d
	fi
	cp -r ./emacs.d/* $HOME/.emacs.d/
	cp ./Xresources $HOME/.Xresources
	if [ ! -d $HOME/bin ] ; then
		mkdir $HOME/bin
		Message="Installed from repo into home directory. Add ~/bin to your path."
	fi
	cp ./epylint $HOME/bin/
	;;
*)
	Message="Unknown command (pull, install)"
	;;
esac

echo $Message

