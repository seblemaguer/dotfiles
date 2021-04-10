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

########################################################################################
#### Basics
########################################################################################
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

# Antigen Bundles
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

# oh-my-zsh
antigen use oh-my-zsh

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
antigen bundle zsh-users/zsh-syntax-highlighting

# Tell Antigen that you're done.
antigen apply


########################################################################################
#### Use .ssh/config for completion
########################################################################################
zstyle -s ':completion:*:hosts' hosts _ssh_config
[[ -r ~/.ssh/config ]] && _ssh_config+=($(cat ~/.ssh/config | sed -ne 's/Host[=\t ]//p'))
zstyle ':completion:*:hosts' hosts $_ssh_config


########################################################################################
#### Aliases / Helpers
########################################################################################
# Some
alias vi='vim'

# Coloring some command
alias mvn="mvn-color"
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

# Network
alias rsync="rsync --protect-args --exclude-from=$HOME/.rsyncignore"

# Emacs helper
alias tangle_emacs_conf="emacs -Q --batch --eval \"(progn (require 'ob-tangle) (dolist (file command-line-args-left) (with-current-buffer (find-file-noselect file) (org-babel-tangle))))\" \"~/.emacs.d/main.org\""

########################################################################################
#### Options
########################################################################################
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

########################################################################################
#### Emacs/Vterm specifics
########################################################################################

if [[ "$INSIDE_EMACS" = 'vterm' ]]
then
    vterm_printf(){
        if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ] ); then
            # Tell tmux to pass the escape sequences through
            printf "\ePtmux;\e\e]%s\007\e\\" "$1"
        elif [ "${TERM%%-*}" = "screen" ]; then
            # GNU screen (screen, screen-256color, screen-256color-bce)
            printf "\eP\e]%s\007\e\\" "$1"
        else
            printf "\e]%s\e\\" "$1"
        fi
    }

    vterm_cmd() {
        local vterm_elisp
        vterm_elisp=""
        while [ $# -gt 0 ]; do
            vterm_elisp="$vterm_elisp""$(printf '"%s" ' "$(printf "%s" "$1" | sed -e 's|\\|\\\\|g' -e 's|"|\\"|g')")"
            shift
        done
        vterm_printf "51;E$vterm_elisp"
    }

    alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'

    if [[ -n ${EMACS_VTERM_PATH} ]] && \
        [[ -f ${EMACS_VTERM_PATH}/etc/emacs-vterm-bash.sh ]]
    then
        source ${EMACS_VTERM_PATH}/etc/emacs-vterm-bash.sh
    fi

    # To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
    [[ ! -f ~/.config/zsh/emacs_p10k.zsh ]] || source ~/.config/zsh/emacs_p10k.zsh
else
    # To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
    [[ ! -f ~/.config/zsh/default_p10k.zsh ]] || source ~/.config/zsh/default_p10k.zsh
fi

########################################################################################
#### Environment toolks
########################################################################################

## Conda
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/lemagues/environment/local/miniconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/home/lemagues/environment/local/miniconda3/etc/profile.d/conda.sh" ]; then
        . "/home/lemagues/environment/local/miniconda3/etc/profile.d/conda.sh"
    else
        export PATH="/home/lemagues/environment/local/miniconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

## SDKMAN
export SDKMAN_DIR="/home/lemagues/.sdkman"
[[ -s "/home/lemagues/.sdkman/bin/sdkman-init.sh" ]] && source "/home/lemagues/.sdkman/bin/sdkman-init.sh"
