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

## Homebrew packages

`Brewfile` lists the shared packages. Casks and other macOS-only packages
are in its `if OS.mac?` block. Put work and per-machine packages in
`~/.Brewfile.local`, which is not in the repo. This repo is public, so private
taps go in the local file.

- `make brew` installs the packages from both files.
- `brew bundle check --file=Brewfile` shows what is missing or out of date.
- `brew bundle cleanup --file=Brewfile` lists the installed packages that are
  in neither file. Add them to a file, or uninstall them.

## Update a machine

```sh
bin/sync-dotfiles.sh
```

The script pulls the changes and runs `make stow`. `make stow` fetches the
submodules and links new files.

## tmux

`tmux/.config/tmux/oh-my-tmux` is a submodule of
[Oh my tmux!](https://github.com/gpakosz/.tmux), and `tmux.conf` is a link to
its `.tmux.conf`. Put your changes in `tmux.conf.local`.

To move to a newer upstream version:

```sh
git -C tmux/.config/tmux/oh-my-tmux fetch
git -C tmux/.config/tmux/oh-my-tmux checkout origin/master
git commit -am "tmux: update oh-my-tmux"
```

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
