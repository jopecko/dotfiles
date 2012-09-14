# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# Personal aliases and functions.

# Personal environment variables and startup programs should go in
# ~/.bash_profile.  System wide environment variables and startup
# programs are in /etc/profile.  System wide aliases and functions are
# in /etc/bashrc.

if [ -f "/etc/bashrc" ] ; then
  source /etc/bashrc
fi

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
#if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
#    debian_chroot=$(cat /etc/debian_chroot)
#fi

# set a fancy prompt (non-color, unless we know we "want" color)
#case "$TERM" in
#    xterm-color) color_prompt=yes;;
#esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

#if [ -n "$force_color_prompt" ]; then
#    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
        # We have color support; assume it's compliant with Ecma-48
        # (ISO/IEC-6429). (Lack of such support is extremely rare, and such
        # a case would tend to support setf rather than setaf.)
#        color_prompt=yes
#    else
#        color_prompt=
#    fi
#fi

#if [ "$color_prompt" = yes ]; then
#    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\#[\033[01;34m\]\w\[\033[00m\]\$ '
#else
#    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
#fi
#unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
#case "$TERM" in
#xterm*|rxvt*)
#     PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
#     ;;
# *)
#     ;;
# esac

# enable color support of ls and also add handy aliases

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

[ -f ~/.bash_aliases ] && source ~/.bash_aliases
[ -f ~/.bash_functions ] && source ~/.bash_functions
[ -f ~/.bash_completions ] && source ~/.bash_completions

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

[ -f /etc/bash.bashrc ] && . /etc/bash.bashrc
[ -f $HOME/.java_profile ] && . $HOME/.java_profile
[ -f $HOME/.env_vars ] && . $HOME/.env_vars # machine-dependent env vars

if [ -f ~/.env ]; then
  . ~/.env
fi

for file in $(ls ~/.bash); do
  . ~/.bash/$file
done

# Add some useful directories to our path
paths="."
paths="$paths $HOME/bin"
paths="$paths /usr/local/sbin"

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

PATH=`echo $PATH | sed -e 's/^\://' -e 's/\:/:/g'`

# our path takes precedence over the one defined by the
# parent process; therefore place the original at the end
PATH="$newpath:$PATH"
export PATH

# http://help.github.com/working-with-key-passphrases/
SSH_ENV="$HOME/.ssh/environment"

function start_agent {
    echo "Initializing new SSH agent..."
    /usr/bin/ssh-agent | sed 's/^echo/#echo/' >| "${SSH_ENV}"
    echo succeeded
    chmod 600 "${SSH_ENV}"
    . "${SSH_ENV}" > /dev/null
    /usr/bin/ssh-add;
}

# Source SSH settings, if applicable
if [ -f "${SSH_ENV}" ]; then
    . "${SSH_ENV}" > /dev/null
    #ps ${SSH_AGENT_PID} doesn't work under cywgin
    ps -ef | grep ${SSH_AGENT_PID} | grep ssh-agent$ > /dev/null || {
        start_agent;
    }
else
    start_agent;
fi

# System-specific configuration
[ -f ${HOME}/.$(hostname).env ] && . ${HOME}/.$(hostname).env

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
