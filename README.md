# dotfiles

> emacs, fish, tmux, ghostty, etc.

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


