#!/usr/bin/env bash
# Pull the latest dotfiles and link any new files into $HOME.
set -euo pipefail
cd "$(dirname "$0")/.."

git pull --rebase --autostash
make stow
