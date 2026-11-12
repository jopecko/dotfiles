#!/usr/bin/env bash
# Run shellcheck on every tracked shell file. zsh scripts are skipped,
# because shellcheck does not support zsh.
set -euo pipefail
cd "$(dirname "$0")/.."

scripts=()
while IFS= read -r file; do
    case "$file" in
        bash/*) ;;
        *) head -n 1 "$file" | grep -qE '^#!.*[/ ](ba)?sh( |$)' && scripts+=("$file") ;;
    esac
done < <(git ls-files)

status=0
shellcheck -S warning -s bash bash/.bashrc bash/.bash_profile bash/.bash/* || status=1
shellcheck -S warning -s sh bash/.profile || status=1
shellcheck -S warning "${scripts[@]}" || status=1

if [ "$status" -eq 0 ]; then
    echo "shellcheck: no warnings"
fi
exit "$status"
