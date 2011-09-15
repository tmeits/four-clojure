#!/bin/sh
# Setting up Emacs & Clojure with Emacs Starter Kit 
# lein install
# Emacs Starter Kit install for ubuntu
git clone http://github.com/technomancy/emacs-starter-kit.git ~/Dropbox/emacs-starter-kit
ln -s ~/Dropbox/emacs-starter-kit ~/.emacs.d
# start Emacs
# emacs -q -l ~/Dropbox/emacs-starter-kit/init.el&
emacs
# setup clojure-mode
# M-x package-list-package
# clojure-mode i x
# M-x clojur-jack-in to begin a Slime session for the current project.
