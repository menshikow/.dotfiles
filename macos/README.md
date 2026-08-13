# macOS

## Setting up a new Mac

```console
$ # install Xcode Command Line Tools
$ xcode-select --install
$ # install Homebrew
$ /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
$ # clone the dotfiles repo
$ git clone git@github.com:menshikow/.dotfiles.git ~/.dotfiles
$ # install everything in the Brewfile
$ cd ~/.dotfiles/macos
$ brew bundle
$ # set macOS defaults (requires restart)
$ ./set-defaults.sh
$ cd ..
$ # bootstrap dotfiles with stow
$ stow .
$ # copy SSH key into ~/.ssh, then:
$ chmod 0600 ~/.ssh/id_rsa
$ # copy fonts (optional)
$ ~/bin/install-fonts.sh
$ # reboot
$ sudo reboot
```