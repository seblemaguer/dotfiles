#!/bin/zsh

# Linking !
# rm -rf ~/.vimrc; ln -s $PWD/vimrc  ~/.vimrc

if [ ! -e $HOME/.vim/bundle ]
then
    mkdir -p $HOME/.vim/bundle

    # link bundle part !
    git clone https://github.com/VundleVim/Vundle.vim.git $HOME/.vim/bundle/vundle

    # Prepare powerline
    FONT_CONF_DIR=$HOME/.cache/fontconfig/
    wget https://raw.githubusercontent.com/powerline/powerline/develop/font/10-powerline-symbols.conf
    mkdir -p $FONT_CONF_DIR
    mv 10-powerline-symbols.conf $FONT_CONF_DIR

    FONT_DIR=$HOME/.local/share/fonts/
    wget https://github.com/powerline/powerline/raw/develop/font/PowerlineSymbols.otf
    mkdir -p $FONT_DIR
    mv PowerlineSymbols.otf $FONT_DIR

    fc-cache -vf $FONT_DIR

    # Prepare wiki path
    mkdir -p $HOME/.vim/vimwiki

    # Install bundles !
    vim +PluginInstall +qall
fi
