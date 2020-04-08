#!/bin/sh
#-*- mode: sh; -*-

###################################################################################
### Tools variable
###################################################################################

# Python part
export PYTHON=python3

# Perl part
export PERL5LIB=$HOME/environment/local/perl:$HOME/work/tools/local/perl:$HOME/work/maintained_tools/local/perl:$PERL5LIB
export PERL5LIB=$PERL5LIB:$HOME/environment/local/perl/lib/perl5/darwin-thread-multi-2level/
export PERL_MB_OPT="--install_base \"$HOME/environment/local/perl\""
export PERL_MM_OPT="INSTALL_BASE=$HOME/environment/local/perl"

# Groovy part
export GROOVY_HOME="/usr/share/groovy"

# Go part
export GOPATH=$HOME/environment/local/go

# NPM part
NPM_PACKAGES="${HOME}/environment/local/npm_packages"
export NODE_PATH="$NPM_PACKAGES/lib/node_modules:$NODE_PATH"

###################################################################################
### Path variables
###################################################################################

# Main path variable
export PATH=${HOME}/bin:${HOME}/environment/local/bin:$PATH
export PATH=$HOME/work/tools/local/bin/:$HOME/work/maintained_tools/local/bin/:$PATH
export PATH=$HOME/work/tools/local/share/matlab/bin:$PATH
export PATH=$HOME/.local/bin:$PATH
export PATH=$HOME/environment/local/go/bin:$PATH
export PATH=$NPM_PACKAGES/bin:$PATH

# Libraries
export DYLD_LIBRARY_PATH=$HOME/environment/local/lib:$HOME/work/tools/local/lib:$HOME/work/maintained_tools/local/lib:/usr/local/opt/icu4c/lib/:$HOME/.local/lib:$DYLD_LIBRARY_PATH
export LD_LIBRARY_PATH=$HOME/environment/local/lib:$HOME/work/tools/local/lib:$HOME/work/maintained_tools/local/lib:/usr/local/opt/icu4c/lib/:$HOME/.local/lib:$LD_LIBRARY_PATH
export LIBRARY_PATH=$HOME/environment/local/lib:$HOME/work/tools/local/lib:$HOME/work/maintained_tools/local/lib:/usr/local/opt/icu4c/lib/:$HOME/.local/lib:$LIBRARY_PATH
export PKG_CONFIG_PATH=$HOME/environment/local/lib/pkgconfig:$HOME/work/tools/local/lib/pkgconfig:$HOME/work/maintained_tools/local/lib/pkgconfig:$PKG_CONFIG_PATH

# Include path
export CPATH=$HOME/environment/local/include:$HOME/work/tools/local/include:$HOME/work/maintained_tools/local/include:/usr/include/jsoncpp/:$CPATH
export C_INCLUDE_PATH=$HOME/environment/local/include:$HOME/work/tools/local/include:$HOME/work/maintained_tools/local/include:/usr/include/jsoncpp/:$C_INCLUDE_PATH
export CPLUS_INCLUDE_PATH=$HOME/environment/local/include:$HOME/work/tools/local/include:$HOME/work/maintained_tools/local/include:/usr/include/jsoncpp/:$CPLUS_INCLUDE_PATH


# Manpage
export MANPATH=$HOME/environment/local/share/man:$HOME/work/tools/local/share/man:$HOME/work/maintained_tools/local/share/man:$NPM_PACKAGES/share/man:$MANPATH
export INFOPATH=$HOME/environment/local/share/info:$INFOPATH


###################################################################################
### Remaining variables
###################################################################################

# Change the locale !
cur_locale=`locale -a | grep utf8  | grep "en_\(IE\|GB\|US\)" | head -n 1`
export LC_ALL=$cur_locale
export LANG=$cur_locale

# Screen detection (for work office)
if [ `which xrandr` != "" ] && [ `hostname` == "stb067" ]
then
    DP_CONNECTED=`xrandr | grep " connected" | grep "HDMI-1" | wc -l`
    if [ $DP_CONNECTED = 1 ]
    then
        ~/.screenlayout/office.sh
    fi
    DP_CONNECTED=`xrandr | grep " connected" | grep "DP-1-1" | wc -l`
    if [ $DP_CONNECTED = 1 ]
    then
        ~/.screenlayout/office_dpi.sh
    fi
fi

# Indicate that the profile is loaded
export PROFILE_LOADED=True
