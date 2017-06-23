#!/bin/zsh

# Linking !
rm -rf ~/.vimrc; ln -s $PWD/vimrc  ~/.vimrc

if [ ! -e $HOME/.vim ]
then
    mkdir -p $HOME/.vim/bundle

    # link bundle part !
    git clone git@github.com:VundleVim/Vundle.vim.git $HOME/.vim/bundle/vundle

    # Prepare powerline
    wget https://raw.githubusercontent.com/powerline/powerline/develop/font/10-powerline-symbols.conf
    mkdir -p $HOME/.fonts.conf.d
    mv 10-powerline-symbols.conf $HOME/.fonts.conf.d
    
    wget https://github.com/powerline/powerline/raw/develop/font/PowerlineSymbols.otf
    mkdir $HOME/.fonts
    mv PowerlineSymbols.otf $HOME/.fonts/PowerlineSymbols.otf

    fc-cache -vf ~/.fonts

    # Prepare wiki path
    mkdir -p $HOME/.vim/vimwiki

    # Install bundles !
    vim +PluginInstall +qall
fi

