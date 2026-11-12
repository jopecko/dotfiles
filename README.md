# dotfiles

My configuration files for macOS and Linux. Each top-level directory is a
[GNU Stow](https://www.gnu.org/software/stow/) package. Stow links the package
contents into `$HOME`.

## Set up a new machine

1. Install `git`, `stow` and `shellcheck`.
2. Clone the repo:

   ```sh
   git clone --recursive git@github.com:jopecko/dotfiles.git ~/.repos/dotfiles
   ```

3. Link the packages. If a real file is in the way, `make` moves it to
   `<file>.backup-<timestamp>` first.

   ```sh
   cd ~/.repos/dotfiles && make
   ```

4. Turn on the pre-commit hook, which runs shellcheck:

   ```sh
   make hooks
   ```

## Machine-local settings

Put secrets and settings for one machine in `~/.<short-hostname>.env`.
`.bashrc` sources this file last. The file is not in the repo.

## Update a machine

```sh
bin/sync-dotfiles.sh
```

The script pulls the changes and runs `make stow`, which links new files.

## Bash layout

`.bashrc` sources the files in `bash/.bash`:

| File | Contents |
|---|---|
| `env` | `PATH` and environment. All shells read it, also non-interactive ones. |
| `options` | Shell options, history and prompt |
| `completion` | Homebrew or system bash-completion |
| `aliases`, `functions` | Aliases and functions |
| `tools` | Setup for optional tools, such as fzf, nvm and sdkman |
| `ssh-agent` | Starts an agent only when no agent is reachable |

`make test` runs shellcheck on all shell files.
