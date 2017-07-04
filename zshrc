#!/bin/zsh

if [[ "$TERM" == "dumb" ]]
then
    unsetopt zle
    unsetopt prompt_cr
    unsetopt prompt_subst
    unfunction precmd
    unfunction preexec
    PS1='$ '
else
    bindkey -e

    #############################################################################################################
    #### Basics
    #############################################################################################################
    if ! type "emacs" > /dev/null; then
        EDITOR=vim
    else
        EDITOR=emacs
    fi

    ZSH=$HOME/.oh-my-zsh
    COMPLETION_WAITING_DOTS="true"


    #############################################################################################################
    #### Plugins
    #############################################################################################################
    fpath=($HOME/.zsh/gradle-completion $fpath)
    plugins=(mercurial svn # Versioning
             git github git-remote-branch gitfast git-extras # Git helpers
             perl python ruby json-tools # Languages
             history-substring-search # helpers
             mvn gradle-completion # Building tools
             cp docker virtualenv gpg-utils adb # some remapping of the system commands
             gnu-utils colored-man rsync sudo # Diverse helpers
             zgen load
            )

    case `uname` in
        Darwin)
            plugins=($plugins brew brew-cask osx) # Mac specific
            ;;
        Linux)
            plugins=($plugins debian ubuntu) # Linux specific
            ;;
    esac

    plugins=($plugins zsh-syntax-highlighting) # Highlighting



    #############################################################################################################
    #### Themes
    #############################################################################################################
    # = FIXME: what is that ?
    if [ "$INSIDE_EMACS" ]
    then
        chpwd() { print -P "\033AnSiTc %d" }
        print -P "\033AnSiTu %n"
        print -P "\033AnSiTc %d"
        ZSH_THEME="rawsyntax"
    else
        ZSH_THEME="bullet-train/bullet-train"
        BULLETTRAIN_PROMPT_ORDER=(
                time
                status
                custom
                context
                dir
                perl
                ruby
                virtualenv
                # nvm
                # go
                git
                hg
                cmd_exec_time
        )
    fi



    #############################################################################################################
    #### Apply oh-my-zsh as the configuration is done
    #############################################################################################################
    source $ZSH/oh-my-zsh.sh


    #############################################################################################################
    #### Environement variables
    #############################################################################################################
    export VIMRUNTIME=`vim -e -T dumb --cmd 'exe "set t_cm=\<C-M>" | echo $VIMRUNTIME | quit' | tr -d '\015' `

    # Common profile stuff
    source ~/.profile

    # Private stuffs
    if [ -e ~/.private_variables ]
    then
        source ~/.private_variables
    fi

    # Coli variable
    if [ -e "/etc/profile.d/coli-path.sh" ]
    then
        source /etc/profile.d/coli-path.sh
        source /etc/profile.d/coli-proxy.sh
        source /etc/profile.d/coli-print.sh
    fi

    #############################################################################################################
    #### Aliases
    #############################################################################################################
    alias vi='vim'
    alias v='vim'
    alias e='emacs'
    alias mvn="mvn-color"

    # = Color
    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
    alias diff=colordiff
    alias less="$VIMRUNTIME/macros/less.sh"

    #
    alias rsync="rsync --exclude-from=$HOME/.rsyncignore"

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

    bindkey '\eOA' history-substring-search-up
    bindkey '\eOB' history-substring-search-down

    # = Diverse
    AUTO_CD=true
fi

#############################################################################################################
#### Machine specific
#############################################################################################################
# Gradle part (move home for word cluster)
if [ `hostname | grep -c "\(falken\|jones\)-"` -ge 1 ]
then
    export GRADLE_USER_HOME=/local/slemaguer/conf/gradle/
fi


#THIS MUST BE AT THE END OF THE FILE FOR GVM TO WORK!!!
[[ -s "$HOME/.gvm/bin/gvm-init.sh" ]] && source "$HOME/.gvm/bin/gvm-init.sh"
