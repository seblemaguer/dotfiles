#!/bin/zsh

# Load profile
if [[ "$PROFILE_LOADED" != "True" ]]
then
    source ~/.profile
fi

if [[ "$TERM" == "dumb" ]]
then
    unsetopt zle
    unsetopt prompt_cr
    unsetopt prompt_subst
    unfunction precmd
    unfunction preexec
    PS1='$ '
    return
fi

bindkey -e

export TERM="xterm-256color"

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi


#############################################################################################################
#### Basics
#############################################################################################################
if ! type "emacs" > /dev/null; then
    export EDITOR=vim
else
    export EDITOR=emacs
fi

# Load Antigen
export ANTIGEN="$HOME/.antigen/"
if [[ ! -d $ANTIGEN ]]; then
  git clone https://github.com/zsh-users/antigen.git $ANTIGEN
fi
source $ANTIGEN/antigen.zsh

# oh-my-zsh
antigen use oh-my-zsh

# VCS bundle
antigen bundle git
antigen bundle github
antigen bundle unixorn/bitbucket-git-helpers.plugin.zsh
antigen bundle denolfe/zsh-travis

# Aliases
antigen bundle djui/alias-tips

# Completion/helpers
antigen bundle zsh-users/zsh-completions
antigen bundle zsh-users/zsh-autosuggestions
antigen bundle sudo
antigen bundle command-not-found

#
# Antigen Bundles
#
antigen bundle tmuxinator
antigen bundle rupa/z

antigen bundle docker
antigen bundle rsync

antigen bundle colored-man-pages
antigen bundle kennethreitz/autoenv

# For SSH, starting ssh-agent is annoying
antigen bundle ssh-agent

# Node Plugins
antigen bundle coffee
antigen bundle node
antigen bundle npm
antigen bundle grunt

# Python Plugins
antigen bundle pip
antigen bundle python
antigen bundle virtualenv

# Java & gradle
antigen bundle gradle/gradle-completion
antigen bundle gvm

# Theme
antigen theme romkatv/powerlevel10k

# Distro specific
# OS specific plugins (comming from https://github.com/seagle0128/dotfiles/blob/master/.zshrc)
if [[ $OSTYPE == darwin* ]]; then
    antigen bundle brew
    antigen bundle brew-cask
    antigen bundle osx
elif [[ $OSTYPE == linux* ]]; then
    if command -v apt-get >/dev/null 2>&1; then
        antigen bundle ubuntu
        alias agua='aguu -y && agar -y && aga -y'
    elif command -v pacman >/dev/null 2>&1; then
        antigen bundle archlinux
    fi
fi

# Highlighting
antigen bundle HeroCC/LS_COLORS
antigen bundle zsh-users/zsh-syntax-highlighting

# Tell Antigen that you're done.
antigen apply


#############################################################################################################
#### Use .ssh/config for completion
#############################################################################################################
zstyle -s ':completion:*:hosts' hosts _ssh_config
[[ -r ~/.ssh/config ]] && _ssh_config+=($(cat ~/.ssh/config | sed -ne 's/Host[=\t ]//p'))
zstyle ':completion:*:hosts' hosts $_ssh_config


#############################################################################################################
#### Environement variables
#############################################################################################################
export VIMRUNTIME=`vim -e -T dumb --cmd 'exe "set t_cm=\<C-M>" | echo $VIMRUNTIME | quit' | tr -d '\015' `

#############################################################################################################
#### Aliases / Helpers
#############################################################################################################
# Some
alias vi='vim'

# Coloring some command
alias mvn="mvn-color"
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
alias less="$VIMRUNTIME/macros/less.sh"

# Network
alias rsync="rsync --protect-args --exclude-from=$HOME/.rsyncignore"

# Emacs helper
alias tangle_emacs_conf="emacs -Q --batch --eval \"(progn (require 'ob-tangle) (dolist (file command-line-args-left) (with-current-buffer (find-file-noselect file) (org-babel-tangle))))\" \"~/.emacs.d/main.org\""

# Keyboard switch french/bulgarian
setxkbmap -layout "fr,bg" -variant ",phonetic" -option "grp:ctrls_toggle"

#############################################################################################################
#### Options
#############################################################################################################
# = History
HISTSIZE=1000
SAVEHIST=1000
HISTFILE=~/.history
APPEND_HISTORY=true
HIST_EXPIRE_DUPS_FIRST=true
HIST_ALLOW_CLOBBER=true
HIST_IGNORE_SPACE=true
HIST_SAVE_NO_DUPS=true

# = Diverse
AUTO_CD=true

#############################################################################################################
#### Conda
#############################################################################################################
[ -f $HOME/environment/local/miniconda3/etc/profile.d/conda.sh ] && source $HOME/environment/local/miniconda3/etc/profile.d/conda.sh
[[ -s "$HOME/.gvm/bin/gvm-init.sh" ]] && source "$HOME/.gvm/bin/gvm-init.sh"

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.config/zsh/p10k.zsh ]] || source ~/.config/zsh/p10k.zsh
