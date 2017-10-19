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
        export EDITOR=vim
    else
        export EDITOR=emacs
    fi

    # Load Antigen
    if [ ! -e ~/.antigen.zsh ]
    then
        curl -L git.io/antigen > ~/.antigen.zsh
    fi
    source ~/.antigen.zsh

    # Load various lib files
    antigen use oh-my-zsh
    # antigen bundle robbyrussell/oh-my-zsh lib/

    # VCS bundle
    antigen bundle git
    antigen bundle github


    antigen bundle djui/alias-tips

    antigen bundle HeroCC/LS_COLORS
    antigen bundle trapd00r/zsh-syntax-highlighting-filetypes
    antigen bundle zsh-users/zsh-syntax-highlighting

    #
    # Antigen Bundles
    #
    antigen bundle tmuxinator
    antigen bundle zsh-users/zsh-completions
    antigen bundle zsh-users/zsh-autosuggestions
    antigen bundle rupa/z
    antigen bundle command-not-found

    antigen bundle docker
    antigen bundle rsync
    antigen bundle sudo

    antigen bundle colored-man-pages
    antigen bundle kennethreitz/autoenv

    # For SSH, starting ssh-agent is annoying
    antigen bundle ssh-agent

    # Node Plugins
    antigen bundle coffee
    antigen bundle node
    antigen bundle npm

    # Python Plugins
    antigen bundle pip
    antigen bundle python
    antigen bundle virtualenv

    # Java & gradle
    antigen bundle gradle/gradle-completion


    # Theme
    antigen theme bhilburn/powerlevel9k powerlevel9k
    # antigen theme caiogondim/bullet-train-oh-my-zsh-theme bullet-train


    # Distro specific
    antigen bundle archlinux

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
