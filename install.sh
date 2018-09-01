#!/bin/zsh

# Install dotfiles
pip install --user dotfiles

# Sync the files
dotfiles -C dotfilesrc --sync

# Post installation (vim part)
zsh vim/install.sh
