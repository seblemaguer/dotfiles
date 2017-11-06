#!/bin/sh
#-*- mode: sh; -*-

# Main path variable
export PATH=${HOME}/environment/local/bin:$HOME/work/tools/local/bin/:$HOME/work/maintained_tools/local/bin/:$PATH
export PATH=$HOME/work/tools/local/share/matlab/bin:$PATH
export PATH=$HOME/.local/bin:$PATH
export PATH=$HOME/work/tools/src/festival/festival-2.4/bin/:$HOME/work/tools/src/speech_tools/speech_tools-2.4/bin/:$PATH

# Libraries
export DYLD_LIBRARY_PATH=$HOME/environment/local/lib:$HOME/work/tools/local/lib:$HOME/work/maintained_tools/local/lib:/usr/local/opt/icu4c/lib/:$HOME/.local/lib:$DYLD_LIBRARY_PATH
export LD_LIBRARY_PATH=$HOME/environment/local/lib:$HOME/work/tools/local/lib:$HOME/work/maintained_tools/local/lib:/usr/local/opt/icu4c/lib/:$HOME/.local/lib:$LD_LIBRARY_PATH
export LIBRARY_PATH=$HOME/environment/local/lib:$HOME/work/tools/local/lib:$HOME/work/maintained_tools/local/lib:/usr/local/opt/icu4c/lib/:$HOME/.local/lib:$LIBRARY_PATH
export PKG_CONFIG_PATH=$HOME/environment/local/lib/pkgconfig:$HOME/work/tools/local/lib/pkgconfig:$HOME/work/maintained_tools/local/lib/pkgconfig:$PKG_CONFIG_PATH

# Include path
export CPATH=$HOME/environment/local/include:$HOME/work/tools/local/include:$HOME/work/maintained_tools/local/include:/usr/include/jsoncpp/:$CPATH
export C_INCLUDE_PATH=$HOME/environment/local/include:$HOME/work/tools/local/include:$HOME/work/maintained_tools/local/include:/usr/include/jsoncpp/:$C_INCLUDE_PATH
export CPLUS_INCLUDE_PATH=$HOME/environment/local/include:$HOME/work/tools/local/include:$HOME/work/maintained_tools/local/include:/usr/include/jsoncpp/:$CPLUS_INCLUDE_PATH

# Python part
export PYTHONPATH=$HOME/work/tools/env/lib/python3.4/site-packages:$HOME/work/tools/local/python:$HOME/work/maintained_tools/local/python:$HOME/work/tools/env/lib/python2.7/site-packages:$HOME/environment/local/lib/python2.7/site-packages/:$PYTHONPATH
export PYTHON=python3

# Perl part
export PERL5LIB=$HOME/environment/local/perl:$HOME/work/tools/local/perl:$HOME/work/maintained_tools/local/perl:$PERL5LIB
export PERL5LIB=$PERL5LIB:$HOME/environment/local/perl/lib/perl5/darwin-thread-multi-2level/
export PERL_MB_OPT="--install_base \"$HOME/environment/local/perl\""
export PERL_MM_OPT="INSTALL_BASE=$HOME/environment/local/perl"

# Groovy part
export GROOVY_HOME="/usr/share/groovy"

# R part
export R_LIBS_USER=$HOME/environment/local/R

# Manpage
export MANPATH=$HOME/environment/local/share/man:$HOME/work/tools/local/share/man:$HOME/work/maintained_tools/local/share/man:$MANPATH
export INFOPATH=$HOME/environment/local/share/info:$INFOPATH

# Change the locale !
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
