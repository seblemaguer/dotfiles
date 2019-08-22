#!/bin/zsh

# Install dotfiles
pip install --user dotfiles

# Sync the files
dotfiles -C dotfilesrc --sync --force

# Post installation (vim part)
zsh vim/install.sh

# Compilation of emacs
emacs -Q --batch --eval "
    (progn
      (require 'ob-tangle)
      (dolist (file command-line-args-left)
        (with-current-buffer (find-file-noselect file)
          (org-babel-tangle))))" "~/.emacs.d/main.org"

