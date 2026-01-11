# Homebrew
[[ -f /opt/homebrew/bin/brew ]] && eval "$(/opt/homebrew/bin/brew shellenv)"

# PATHs
export PATH="$HOME/.local/bin:$HOME/.opencode/bin:$BUN_INSTALL/bin:$PATH"
export PATH="/opt/homebrew/opt/tree-sitter@0.25/libexec/bin:/Library/TeX/texbin:$PATH"

# Oh My Zsh
export ZSH="$HOME/.oh-my-zsh"
ZSH_THEME="edvardm"
plugins=(git zsh-autosuggestions zsh-syntax-highlighting)
source $ZSH/oh-my-zsh.sh

# Node (NVM)
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"

# Go (GVM)
[[ -s "$HOME/.gvm/scripts/gvm" ]] && source "$HOME/.gvm/scripts/gvm"

# Python (Pyenv)
export PYENV_ROOT="$HOME/.pyenv"
command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"

# ASDF
[ -f "$HOME/.asdf/asdf.sh" ] && . "$HOME/.asdf/asdf.sh"

# Bun
export BUN_INSTALL="$HOME/.bun"
[ -s "$BUN_INSTALL/_bun" ] && source "$BUN_INSTALL/_bun"

# Haskell (GHCup)
[ -f "$HOME/.ghcup/env" ] && . "$HOME/.ghcup/env"

# pnpm
export PNPM_HOME="$HOME/Library/pnpm"
export PATH="$PNPM_HOME:$PATH"

# Conda
__conda_setup="$('/opt/miniconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    [ -f "/opt/miniconda3/etc/profile.d/conda.sh" ] && . "/opt/miniconda3/etc/profile.d/conda.sh"
    export PATH="/opt/miniconda3/bin:$PATH"
fi
unset __conda_setup

# Opam
[[ -r "$HOME/.opam/opam-init/init.zsh" ]] && source "$HOME/.opam/opam-init/init.zsh" > /dev/null 2>&1

# Aliases
alias u='cursor'
alias c='code --reuse-window'
alias vim='nvim'
alias vi='nvim'
alias ll='ls -la'
alias gs='git status'
alias gd='git diff'
alias ..='cd ..'
alias reload='source ~/.zshrc'

# History
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.zsh_history
setopt SHARE_HISTORY HIST_IGNORE_DUPS HIST_IGNORE_ALL_DUPS

# Completion Caching
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/cache

# Zsh corrections
ENABLE_CORRECTION="true"
CASE_SENSITIVE="false"
