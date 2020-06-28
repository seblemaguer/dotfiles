#!/bin/bash


########################################################################################
#### Tramp emacs mandatory step
########################################################################################
case "$TERM" in
    "dumb")
        export PS1="> "
        return
        ;;
    xterm*|rxvt*|eterm*|screen*)
        tty -s && export PS1="$ "
        ;;
esac

########################################################################################
#### Load profile
########################################################################################
if [[ "$PROFILE_LOADED" != "True" ]]
then
    source ~/.profile
fi


########################################################################################
#### Environment variables
########################################################################################
if [[ `which emacs` = "" ]]; then
    export EDITOR=vim
else
    export EDITOR=emacs
fi

########################################################################################
#### Aliases
########################################################################################

# Some utilities
alias svim='sudo vim'
alias h='cd ~'
alias ..='cd ..'
alias cd..='cd ..'
alias ...='cd ../..'
alias cim='vim'
alias root='sudo su'
alias dfh='df -h'

# = adding colors Color
alias ls='ls -Ch --color=auto'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

# Cluster aliases
alias msqueue='squeue -o "%.8i %.9P %.20j %.8u %.8T %.10M %.12l %.8C %.15b %.15R"'
alias check_queue="msqueue | sed 's/[ \t]\+/ /g' | cut -d' ' -f5,6,10 | sed 's/:.*//g' | sort | uniq -c | grep -v 'USER STATE' | sed 's/^[ ]*//g' | sort -h"

########################################################################################
#### Prompt
########################################################################################

BGREEN='\[\033[1;32m\]'
GREEN='\[\033[0;32m\]'
BRED='\[\033[1;31m\]'
RED='\[\033[0;31m\]'
BBLUE='\[\033[1;34m\]'
BLUE='\[\033[0;34m\]'
NORMAL='\[\033[00m\]'
TIME=$(date +%H:%M)

if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ];
then
    PS1="${GREEN}[\A] ${NORMAL}\u:${BLUE}\h@${RED}\w ${BLUE}: ${NORMAL}"
else
    PS1="${GREEN}[\A] ${NORMAL}\u@${RED}\w ${BLUE}: ${NORMAL}"
fi

########################################################################################
#### History
########################################################################################
export HISTCONTROL=erasedups:ignorespace
export HISTIGNORE="cd:ls:[bf]g:clear"
if [[ "$(set -o | grep 'emacs\|\bvi\b' | cut -f2 | tr '\n' ':')" != 'off:off:' ]]; then
    bind '"\M-w"':"\"\C-k\C-ahistory | grep '^ *[0-9]* *\C-e.'\C-m\""
    bind '"\e[A"':history-search-backward
    bind '"\e[B"':history-search-forward
fi

########################################################################################
#### Titles
########################################################################################
case "$TERM" in
    xterm*|rxvt*)
        PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD}\007"'
        ;;
    *)
        ;;
esac


########################################################################################
#### Completion
########################################################################################

if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

########################################################################################
#### Functions
########################################################################################

# a utiliser avec des echo $_bold et echo $_normal
_bold=$(tput bold)
_normal=$(tput sgr0)

# imprime les infos du gestionnaire de version en cours
# si on en trouve un dans le pwd
__vcs_dir() {
    local vcs base_dir sub_dir ref
    sub_dir() {
        local sub_dir
        sub_dir=$(readlink -f "${PWD}")
        sub_dir=${sub_dir#$1}
        echo ${sub_dir#/}
    }
    # git
    git_dir() {
        base_dir=$(git rev-parse --show-cdup 2>/dev/null) || return 1
        if [ -n "$base_dir" ]; then
            base_dir=`cd $base_dir; pwd`
        else
            base_dir=$PWD
        fi
        sub_dir=$(git rev-parse --show-prefix)
        sub_dir="/${sub_dir%/}"
        ref=$(git symbolic-ref -q HEAD || git name-rev --name-only HEAD 2>/dev/null)
        ref=${ref#refs/heads/}
        vcs="git"
    }
    # subversion
    svn_dir() {
        [ -d ".svn" ] || return 1
        base_dir="."
        while [ -d "$base_dir/../.svn" ]; do
            base_dir="$base_dir/.."
        done
        base_dir=`cd $base_dir; pwd`
        sub_dir="/$(sub_dir "${base_dir}")"
        ref=$(svn info "$base_dir" | awk '/^URL/ { sub(".*/","",$0); r=$0 } /^Revision/ { sub("[^0-9]*","",$0); print r":"$0 }')
        vcs="svn"
        alias pull="svn up"
        alias commit="svn commit"
        alias push="svn ci"
        alias revert="svn revert"
    }
    # mercurial
    hg_dir() {
        base_dir="."
        while [ ! -d "$base_dir/.hg" ]; do
            base_dir="$base_dir/.."
            [ $(readlink -f "${base_dir}") = "/" ] && return 1
        done
        base_dir=$(readlink -f "$base_dir")
        sub_dir="/$(sub_dir "${base_dir}")"
        ref=$(< "${base_dir}/.hg/branch")
        hgqtop=$(hg qtop)
        if [[ $hgqtop == 'No patches applied' ]]; then
            extra=""
        else
            extra=" >> $hgqtop"
        fi
        vcs="hg"
    }
    hg_dir || git_dir || svn_dir || base_dir="$PWD"
    echo "${vcs:+($vcs)}${_bold}${base_dir/$HOME/~}${_normal}${vcs:+[$ref]${_bold}${sub_dir}${_normal}$extra}"
}

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/lemagues/environment/local/miniconda3/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
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

