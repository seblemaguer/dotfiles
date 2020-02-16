#!/bin/sh -e

# Create needed link
echo "Linking emacs.d to $HOME/.emacs.d"
ln -s $PWD/emacs.d $HOME/.emacs.d

ls -l $HOME/.emacs.d

# Tangling
echo "Attempting to tangle main.org..."
${EMACS:=emacs} -Q --batch --eval "
    (progn
      (require 'ob-tangle)
      (dolist (file command-line-args-left)
        (with-current-buffer (find-file-noselect file)
          (org-babel-tangle))))" "~/.emacs.d/main.org"

# Loading
echo "Attempting to startup..."
${EMACS:=emacs} --debug-init -nw -Q --batch -L $HOME/.emacs.d --eval "(require 'init)"

# Yahoo success (normally XD)
echo "Startup successful"
