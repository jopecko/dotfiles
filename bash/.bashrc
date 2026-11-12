#!/bin/bash
# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# Personal environment variables and startup programs should go in
# ~/.bash_profile.  System wide environment variables and startup
# programs are in /etc/profile.  System wide aliases and functions are
# in /etc/bashrc.


# if not running interactively, don't do anything
case $- in
	*i*) ;;
	*) return;;
esac

# Case-insensitive globbing (used in pathname expansion)
shopt -s nocaseglob

# Append to the Bash history file, rather than overwriting it
shopt -s histappend

# Autocorrect typos in path names when using `cd`
shopt -s cdspell

# Enable some Base 4 features when possible:
# * `autocd`, e.g., `**/qux` will enter `./foo/bar/baz/qux`
# * Recursive globbing, e.g. `echo **/*.txt`
for option in autocd globstar; do
    shopt -s "$option" 2> /dev/null
done

# Add tab completion for SSH hostnames based on ~/.ssh/config
# ignoring wildcards
[[ -e "$HOME/.ssh/config" ]] && complete -o "default" \
    -o "nospace" \
    -W "$(grep "^Host" ~/.ssh/config | \
    grep -v "[?*]" | cut -d " " -f2 | \
    tr ' ' '\n')" scp sftp ssh 

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

if [ -f "/etc/bashrc" ] ; then
	# shellcheck source=/dev/null
	source /etc/bashrc
fi

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
  source ~/.env
fi

# path goes first so aliases can detect Homebrew tools like eza.
# Later files override earlier ones; exports must stay after config.
for file in path aliases completion config exports functions; do
    # shellcheck source=/dev/null
    source ~/.bash/"$file"
done
unset file

# Add some useful directories to our path
paths="$HOME/bin"
paths="$paths /usr/local/sbin"

# I commonly install utilities in $HOME/opt; find any
# bin directories therein and add them to paths
for binDir in `find $HOME/opt -type d -name bin`; do
    paths="$paths $binDir"
done

for path in $paths; do
    add_to_path "$path"
done
unset path paths binDir
export PATH

# System-specific configuration
host="${HOSTNAME%%.*}"
[ -f ${HOME}/.${host}.env ] && . ${HOME}/.${host}.env
