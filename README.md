# dotfiles

> fish, nvim, tmux, ghostty, macOS defaults and brew bundle, etc.

Managed with [GNU stow](https://www.gnu.org/software/stow/) — the repo IS the
stow tree, so `stow .` from the repo root links everything into `~`.

## Requirements

- `git`
- `stow` (`brew install stow`)
- `fish` (the shell)

## Install

```console
$ git clone git@github.com:menshikow/.dotfiles.git ~/.dotfiles
$ cd ~/.dotfiles
$ stow .
```

## Update

```console
$ cd ~/.dotfiles
$ git pull
$ stow .
```

## Fonts

Fonts live in `fonts/` and are **not tracked by git**. Install them with:

```console
$ ~/bin/install-fonts.sh
```

## macOS defaults

```console
$ ~/.dotfiles/macos/set-defaults.sh   # requires a restart
$ cd ~/.dotfiles/macos && brew bundle # install everything in the Brewfile
```

```console
$ ln -s ~/.dotfiles/.config/sioyek/prefs_user.config \
        "$HOME/Library/Application Support/sioyek/prefs_user.config"
```
