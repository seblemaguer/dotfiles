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

    export TERM="xterm-256color"
    #############################################################################################################
    #### Basics
    #############################################################################################################
    if ! type "emacs" > /dev/null; then
        export EDITOR=vim
    else
        export EDITOR=emacs
    fi

    # Load Antigen
    if [ ! -e ~/.cache/antigen.zsh ]
    then
        curl -L git.io/antigen > ~/.cache/antigen.zsh
    fi
    source ~/.cache/antigen.zsh

    # Load various lib files
    antigen use oh-my-zsh
    # antigen bundle robbyrussell/oh-my-zsh lib/

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
    antigen theme bhilburn/powerlevel9k

    POWERLEVEL9K_MODE='awesome-fontconfig'
    ZSH_THEME="powerlevel9k/powerlevel9k"
    POWERLEVEL9K_COLOR_SCHEME='dark'

    POWERLEVEL9K_PROMPT_ON_NEWLINE=true
    POWERLEVEL9K_MULTILINE_FIRST_PROMPT_PREFIX="%{%F{249}%}\u250f"
    POWERLEVEL9K_MULTILINE_LAST_PROMPT_PREFIX="%{%F{249}%}\u2517%{%F{default}%}❯ "

    POWERLEVEL9K_SHORTEN_DIR_LENGTH=3
    POWERLEVEL9K_SHORTEN_STRATEGY="truncate_middle"

    POWERLEVEL9K_VCS_STAGED_ICON="\u00b1"
    POWERLEVEL9K_VCS_UNTRACKED_ICON="\u25CF"
    POWERLEVEL9K_VCS_UNSTAGED_ICON="\u00b1"
    POWERLEVEL9K_VCS_INCOMING_CHANGES_ICON="\u2193"
    POWERLEVEL9K_VCS_OUTGOING_CHANGES_ICON="\u2191"
    POWERLEVEL9K_SHOW_CHANGESET="true"
    POWERLEVEL9K_CHANGESET_HASH_LENGTH="12"
    POWERLEVEL9K_VCS_MODIFIED_BACKGROUND="clear"
    POWERLEVEL9K_VCS_UNTRACKED_BACKGROUND="clear"
    POWERLEVEL9K_VCS_MODIFIED_FOREGROUND="clear"
    POWERLEVEL9K_VCS_UNTRACKED_FOREGROUND="clear"

    POWERLEVEL9K_DIR_HOME_BACKGROUND="clear"
    POWERLEVEL9K_DIR_HOME_FOREGROUND="blue"
    POWERLEVEL9K_DIR_HOME_SUBFOLDER_BACKGROUND="clear"
    POWERLEVEL9K_DIR_HOME_SUBFOLDER_FOREGROUND="blue"
    POWERLEVEL9K_DIR_DEFAULT_BACKGROUND="clear"
    POWERLEVEL9K_DIR_DEFAULT_FOREGROUND="white"

    POWERLEVEL9K_ROOT_INDICATOR_BACKGROUND="red"
    POWERLEVEL9K_ROOT_INDICATOR_FOREGROUND="white"

    POWERLEVEL9K_TODO_BACKGROUND="clear"
    POWERLEVEL9K_TODO_FOREGROUND="green"

    POWERLEVEL9K_HISTORY_BACKGROUND="clear"
    POWERLEVEL9K_HISTORY_FOREGROUND="blue"

    POWERLEVEL9K_STATUS_OK_BACKGROUND="clear"
    POWERLEVEL9K_STATUS_OK_FOREGROUND="green"
    POWERLEVEL9K_STATUS_ERROR_BACKGROUND="clear"
    POWERLEVEL9K_STATUS_ERROR_FOREGROUND="red"

    POWERLEVEL9K_LOAD_CRITICAL_BACKGROUND="clear"
    POWERLEVEL9K_LOAD_WARNING_BACKGROUND="clear"
    POWERLEVEL9K_LOAD_NORMAL_BACKGROUND="clear"
    POWERLEVEL9K_LOAD_CRITICAL_FOREGROUND="red"
    POWERLEVEL9K_LOAD_WARNING_FOREGROUND="yellow"
    POWERLEVEL9K_LOAD_NORMAL_FOREGROUND="green"
    POWERLEVEL9K_LOAD_CRITICAL_VISUAL_IDENTIFIER_COLOR="red"
    POWERLEVEL9K_LOAD_WARNING_VISUAL_IDENTIFIER_COLOR="yellow"
    POWERLEVEL9K_LOAD_NORMAL_VISUAL_IDENTIFIER_COLOR="green"

    POWERLEVEL9K_RAM_BACKGROUND="clear"
    POWERLEVEL9K_RAM_FOREGROUND="green"
    POWERLEVEL9K_RAM_ELEMENTS="ram_free"

    POWERLEVEL9K_TIME_BACKGROUND="clear"
    POWERLEVEL9K_TIME_FOREGROUND="white"

    POWERLEVEL9K_TIME_FORMAT="%D{\uf017 %H:%M \uf073 %d.%m.%y}"
    POWERLEVEL9K_STATUS_VERBOSE="true"
    POWERLEVEL9K_BATTERY_STAGES=(
        $'▏    ▏' $'▎    ▏' $'▍    ▏' $'▌    ▏' $'▋    ▏' $'▊    ▏' $'▉    ▏' $'█    ▏'
        $'█▏   ▏' $'█▎   ▏' $'█▍   ▏' $'█▌   ▏' $'█▋   ▏' $'█▊   ▏' $'█▉   ▏' $'██   ▏'
        $'██   ▏' $'██▎  ▏' $'██▍  ▏' $'██▌  ▏' $'██▋  ▏' $'██▊  ▏' $'██▉  ▏' $'███  ▏'
        $'███  ▏' $'███▎ ▏' $'███▍ ▏' $'███▌ ▏' $'███▋ ▏' $'███▊ ▏' $'███▉ ▏' $'████ ▏'
        $'████ ▏' $'████▎▏' $'████▍▏' $'████▌▏' $'████▋▏' $'████▊▏' $'████▉▏' $'█████▏'
    )

    POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=('status' 'background_jobs' 'root_indicator' 'dir' 'vcs')
    POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=('load' 'ram' 'battery' 'history' 'todo' 'time')

    # Distro specific
    antigen bundle archlinux

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

    # Keyboard switch
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
fi

#############################################################################################################
#### Machine specific
#############################################################################################################


#THIS MUST BE AT THE END OF THE FILE FOR GVM TO WORK!!!
[[ -s "$HOME/.gvm/bin/gvm-init.sh" ]] && source "$HOME/.gvm/bin/gvm-init.sh"
