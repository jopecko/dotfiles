#!/bin/bash

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

[ -f $HOME/.java_profile ] && . $HOME/.java_profile
[ -f $HOME/.env_vars ] && . $HOME/.env_vars # machine-dependent env vars

if [ -f ~/.env ]; then
  source ~/.env
fi

# path goes first so aliases can detect Homebrew tools like eza.
# Later files override earlier ones; exports must stay after config.
for file in path aliases config exports functions; do
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
