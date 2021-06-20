#!/bin/bash

##########################################################################
## Fonts
##########################################################################
FONT_CONF_DIR=$HOME/.cache/fontconfig/
FONT_DIR=$HOME/.local/share/fonts/
if [ ! -d "$FONT_DIR" ]; then
    echo "mkdir -p $FONT_DIR"
    mkdir -p "$FONT_DIR"
else
    echo "Found fonts dir $FONT_DIR"
fi

if [ ! -d "$FONT_CONF_DIR" ]; then
    echo "mkdir -p $FONT_CONF_DIR"
    mkdir -p "$FONT_CONF_DIR"
else
    echo "Found font configuration dir $FONT_CONF_DIR"
fi


# Powerline
echo "Install powerline fonts"
wget -O $FONT_CONF_DIR/10-powerline-symbols.conf https://raw.githubusercontent.com/powerline/powerline/develop/font/10-powerline-symbols.conf
wget -O $FONT_DIR/PowerlineSymbols.otf https://github.com/powerline/powerline/raw/develop/font/PowerlineSymbols.otf

# Fira
echo "Install Fira fonts"
for type in Bold Light Medium Regular Retina; do
    file_path="$FONT_DIR/FiraCode-${type}.ttf"
    file_url="https://github.com/tonsky/FiraCode/blob/master/distr/ttf/FiraCode-${type}.ttf?raw=true"
    if [ ! -e "${file_path}" ]; then
        echo "  => wget -O $file_path $file_url"
        wget -O "${file_path}" "${file_url}"
    else
	echo "  => Found existing file $file_path"
    fi;
done

echo "fc-cache -f"
fc-cache -f


##########################################################################
## Tangling
##########################################################################

# Tangle all the configuration
for cur_org in $(ls *.org | grep -v README.org)
do
    emacs -Q --batch --eval "
    (progn
      (require 'ob-tangle)
      (dolist (file command-line-args-left)
        (with-current-buffer (find-file-noselect file)
          (org-babel-tangle))))" $cur_org
done

# Tangle emacs
emacs -Q --batch --eval "
    (progn
      (require 'ob-tangle)
      (dolist (file command-line-args-left)
        (with-current-buffer (find-file-noselect file)
          (org-babel-tangle))))" "~/.emacs.d/README.org"


##########################################################################
## Post processing
##########################################################################
echo "Post Processing"
for cur_hook in $(ls postprocessing/*.sh)
do
    echo " ==> Execute hook ${cur_hook}"
    bash $cur_hook
done
