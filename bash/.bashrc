#!/bin/bash
# shellcheck source=/dev/null

[ -r ~/.bash/env ] && . ~/.bash/env

# if not running interactively, don't do anything
case $- in
	*i*) ;;
	*) return;;
esac

[ -f /etc/bashrc ] && . /etc/bashrc

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

for file in ~/.java_profile ~/.env_vars ~/.env; do
    [ -f "$file" ] && . "$file"
done

for file in options completion aliases functions tools ssh-agent; do
    . ~/.bash/"$file"
done
unset file

# Machine-local settings and secrets, kept out of the repo
host="${HOSTNAME%%.*}"
[ -f "$HOME/.$host.env" ] && . "$HOME/.$host.env"
unset host
