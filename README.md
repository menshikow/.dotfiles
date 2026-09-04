The configuration files are managed with GNU Stow. Each top-level directory represents a "group" of configs (e.g., alacritty, fish, ghostty, nvim), and you can "install" (by symlinking) the configs of a group using

$ stow -Sv <group>

You can use -n to just show what it would install.

Stow has a bunch of shortcomings, but I haven't bothered to move to anything else yet. In theory, the directory layout should be mostly compatible with other tools like chezmoi that you may enjoy more, though I have not tried them myself.
