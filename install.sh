#!/bin/zsh

# Sync the document
dotfiles -C dotfilesrc --sync

# Post installation (vim part)
zsh vim/install.sh
