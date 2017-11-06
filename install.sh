#!/bin/zsh

# Dealing with options
while getopts ":j:hs" opt; do
    case $opt in
        j)
            NB_PROC=$OPTARG
            echo "parallel mode activated with $NB_PROC process" >&2
            ;;
        s)
            echo "server mode installation activated" >&2
            SERVER_MODE_ON=true
            ;;
        h)
            echo "An help part should be done" >&2
            ;;
        \?)
            echo "Invalid option: -$OPTARG" >&2
            ;;
    esac
done
shift $OPTIND-1


REL_PATH=$PWD

# - system stuffs
rm -rf ~/.bashrc; ln -s $REL_PATH/system/bashrc ~/.bashrc
rm -rf ~/.zshrc; ln -s $REL_PATH/system/zshrc ~/.zshrc
rm -rf ~/.profile; ln -s $REL_PATH/system/profile ~/.profile
rm -rf ~/.tmux.conf; ln -s $REL_PATH/system/tmux.conf ~/.tmux.conf
rm -rf ~/.config/htop; mkdir -p ~/.config/htop; ln -s $REL_PATH/system/htoprc ~/.config/htop/
rm -rf ~/.rsyncignore; ln -s $REL_PATH/system/rsyncignore ~/.rsyncignore

if [ "$SERVER_MODE_ON" != true ]
then
    rm -rf ~/.ssh/config; ln -s $REL_PATH/system/ssh ~/.ssh/config
fi


# - python
rm -rf ~/.ipython; mkdir ~/.ipython; ln -s $REL_PATH/ipython/ipythonrc ~/.ipython
rm -rf ~/.pylintrc; ln -s $REL_PATH/python/pylintrc ~/.pylintrc

# - emacs, editor and dev stuffs
(cd $REL_PATH/vim; zsh ./install.sh)

if [ "$SERVER_MODE_ON" != true ]
then
    mkdir -p ~/.emacs.d
    rm -rf ~/.emacs ~/.emacs.d/init.el; ln -s $REL_PATH/emacs/init.el ~/.emacs.d/init.el
fi

# - mail stuffs
if [ "$SERVER_MODE_ON" != true ]
then
    case `uname` in
        Darwin)
            rm -rf ~/.mbsyncrc; ln -s $REL_PATH/mail/mbsyncrc_mac ~/.mbsyncrc
            ;;

        *)
            rm -rf ~/.mbsyncrc; ln -s $REL_PATH/mail/mbsyncrc_linux ~/.mbsyncrc
            ;;
    esac

    # Main configs
    rm -rf ~/.get_passwd.py; ln -s $REL_PATH/mail/get_passwd.py ~/.get_passwd.py
    rm -rf ~/.msmtprc; ln -s $REL_PATH/mail/msmtp.rc ~/.msmtprc
    mkdir ~/.msmtp
    rm -rf ~/.imapnotify; ln -s $REL_PATH/mail/imapnotify ~/.imapnotify

    # Auto start
    case `uname` in
        Darwin)
            ;;

        *)
            mkdir -p ~/.config/autostart
            for i in `ls $REL_PATH/mail/autostart`
            do
                rm -rf ~/.config/autostart/$i; ln -s $REL_PATH/mail/autostart/$i ~/.config/autostart/$i
            done
            ;;
    esac
fi


# - VCS
rm -rf ~/.gitconfig; ln -s $REL_PATH/vcs/gitconfig ~/.gitconfig
rm -rf ~/.hgrc; ln -s $REL_PATH/vcs/hgrc ~/.hgrc

# - other stuffs
if [ "$SERVER_MODE_ON" != true ]
then
    rm -rf ~/.abcde.conf; ln -s $REL_PATH/abcde/abcde.conf ~/.abcde.conf
fi

rm -rf ~/.latexmkrc; ln -s $REL_PATH/latexmk/latexmkrc ~/.latexmkrc
rm -rf ~/texmf; ln -s $REL_PATH/latex-packages ~/texmf
