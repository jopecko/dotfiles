# .bashrc
#
# This file is read (normally) by interactive shells only. It serves to define
# elements that are not inherited through the environment, such as aliases,
# functions, etc.
#

[ -f /etc/bashrc ] && . /etc/bashrc
[ -f $HOME/.java_profile ] && . $HOME/.java_profile
[ -f $HOME/.profile ] && . $HOME/.profile
[ -f $HOME/.env_vars ] && . $HOME/.env_vars # machine-dependent env vars

# prompt coloring
# User@Host - working dir
#export PS1="\u@\h \w$ "
#export PS1="\u@\h \w: "
# change PS1 to also show the current git branch
PS1='[\u@\h \W$(__git_ps1 " (%s)")]\$ '

if [ `/usr/bin/whoami` = "root" ]; then
    # root has a red prompt
    export PS1="\[\033[0;31m\]\u@\h \w \$ \[\033[0m\]"
elif [ `hostname` = "puyo" -o `hostname` = "pdp10" -o `hostname` = "dynabook" ] ; then
  # the hosts I use on a daily basis have blue
  export PS1="\[\033[0;36m\]\u@\h \w \$ \[\033[0m\]"
else
  # purple by default
  export PS1="\[\033[0;35m\]\u@\h \w \$ \[\033[0m\]"
fi

# CLI Colors
export CLICOLOR=1
# use yellow for dirs
export LSCOLORS=dxfxcxdxbxegedabagacad

set -o notify    # report status of terminated background jobs immediately
set -o noclobber # don't accidentally overwrite a file using redirection
set -o ignoreeof # don't use ^D to exit
#set -o nounset
#set -o xtrace    # useful for debugging

shopt -s cdspell # Correct minor spelling errors in cd command
shopt -s histappend histreedit
shopt -s cmdhist # save multi-line commands in history as single line
shopt -s dotglob # return files beginning with a dot in path-name expansion

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# history handling
#
# Erase duplicates
export HISTCONTROL=erasedups
# keep 5000 lines in ~/.bash_history (default is 500)
export HISTSIZE=5000
export HISTFILESIZE=5000
# append to bash_history if Terminal.app quits
shopt -s histappend

# ignore duplicate lines next to each other and
# ignore lines with a leading space
export HISTCONTROL=ignoreboth

# avoid having consecutive duplicate commands and other
# not so useful information appended to the history list
export HISTIGNORE="\&:ls:ls *:mutt:[bf]g:exit:man:pws:cd ..: cd ~-:cd -:jobs:set -x"

# add optional tools to the path
# http://github.com/guides/compiling-and-installing-git-on-mac-os-x
for a in local $(ls /opt/ | grep -v local); do
    FULLPATH=/opt/$a
    if [ -x $FULLPATH ]; then
        if [ -x $FULLPATH/bin ]; then
            export PATH="$FULLPATH/bin:$PATH"
        fi
        if [ -x $FULLPATH/sbin ]; then
            export PATH="$FULLPATH/sbin:$PATH"
        fi
        if [ -x $FULLPATH/share/aclocal ]; then
            export ACLOCAL_FLAGS="-I $FULLPATH/share/aclocal $ACLOCAL_FLAGS"
        fi
        if [ -x $FULLPATH/man ]; then
            export MANPATH="$FULLPATH/man:$MANPATH"
        fi
        if [ -x $FULLPATH/share/man ]; then
            export MANPATH="$FULLPATH/share/man:$MANPATH"
        fi
        if [ -x $FULLPATH/lib/pkgconfig ]; then
            export PKG_CONFIG_PATH="$FULLPATH/lib/pkgconfig/:$PKG_CONFIG_PATH"
        fi
    fi
done

# Add some useful directories to out path
paths="."
paths="$paths $HOME/bin"

# I commonly install utilities in $HOME/opt; find any
# bin directories therein and add them to paths
for binDir in `find $HOME/opt -type d -name bin`; do
    paths="$paths $binDir"
done

for path in $paths; do
    # make sure this entry doesn't already exist in the PATH
    if `echo $PATH | egrep $path'(\:|$)' >/dev/null 2>&1`; then
	continue
    fi
    if [ -d $path ]; then
	newpath=$newpath:$path
    fi
done

PATH=`echo $PATH | sed -e 's/^\://' -e 's/\s/:/g'`

# our path takes precedence over the one defined by the
# parent process; therefore place the original at the end
PATH="$newpath:$PATH"
export PATH

# enable programmable completion features
#if [ -f /etc/bash_completion ]; then
#    . /etc/bash_completion
#fi
