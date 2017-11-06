#!/bin/bash


#############################################################################################################
#### Environment variables
#############################################################################################################
export VIMRUNTIME=`vim -e -T dumb --cmd 'exe "set t_cm=\<C-M>"|echo $VIMRUNTIME|quit' | tr -d '\015' `

# Include profile variables
source ~/.profile


if ! type "emacs" > /dev/null; then
    EDITOR=vim
else
    EDITOR=emacs
fi

#############################################################################################################
#### Tramp emacs mandatory step
#############################################################################################################
[ -z "$PS1" ] && return

if [[ "$TERM" == "dumb" ]]
then
    PS1='$ '
fi


#############################################################################################################
#### Aliases
#############################################################################################################

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
alias gcc=colorgcc.pl
alias g++=colorgcc.pl
alias diff=colordiff
alias less="$VIMRUNTIME/macros/less.sh"

#############################################################################################################
#### Prompt
#############################################################################################################

BGREEN='\[\033[1;32m\]'
GREEN='\[\033[0;32m\]'
BRED='\[\033[1;31m\]'
RED='\[\033[0;31m\]'
BBLUE='\[\033[1;34m\]'
BLUE='\[\033[0;34m\]'
NORMAL='\[\033[00m\]'
TIME=$(date +%H:%M)
PS1="${GREEN}[\A]${NORMAL}\u@${RED}\w ${BLUE}: ${NORMAL}"

#############################################################################################################
#### History
#############################################################################################################
bind '"\M-w"':"\"\C-k\C-ahistory | grep '^ *[0-9]* *\C-e.'\C-m\""
bind '"\e[A"':history-search-backward
bind '"\e[B"':history-search-forward
export HISTCONTROL=erasedups:ignorespace
export HISTIGNORE="cd:ls:[bf]g:clear"

#############################################################################################################
#### Titles
#############################################################################################################
case "$TERM" in
    xterm*|rxvt*)
        PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD}\007"'
        ;;
    *)
        ;;
esac


#############################################################################################################
#### Completion
#############################################################################################################

if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

#############################################################################################################
#### Functions
#############################################################################################################

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

#THIS MUST BE AT THE END OF THE FILE FOR GVM TO WORK!!!
[[ -s "$HOME/.gvm/bin/gvm-init.sh" ]] && source "$HOME/.gvm/bin/gvm-init.sh"
