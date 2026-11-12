# Dotfiles

Personal dotfiles, linked into `$HOME` with GNU stow: one package per top-level directory (see `Makefile`).

## Portability

These configs run on both macOS and Linux machines. Every change must work on both:

- Guard platform-specific code by detecting the tool (`command -v pbcopy`) or the OS (`[[ $OSTYPE == darwin* ]]`), with a fallback for the other platform.
- Before removing something as dead, confirm it is dead on Linux too. Things that are no-ops on macOS but live on Debian/Ubuntu: `/etc/bash_completion`, `lesspipe`, `~/.profile` (read by display managers), an ssh-agent started by the shell.

## Machine-local files

Secrets and per-host settings live outside the repo in `~/.<short-hostname>.env`, sourced last by `.bashrc`. Keep their contents out of the repo.

## Verifying shell changes

Snapshot a fresh login shell before and after the change and diff the two; every difference should be intended:

```sh
env -i HOME="$HOME" TERM=xterm-256color PATH=/usr/bin:/bin USER="$USER" bash -lic \
  'echo "$PATH" | tr : "\n"; alias; declare -F; shopt -p; set +o' </dev/null
```

## Commits

Commit directly to `master`.
