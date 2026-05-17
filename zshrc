# =========================
# powerlevel10k instant prompt
# =========================
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# =========================
# PATH and Homebrew
# =========================
# Local bins
export PATH="$HOME/.local/bin:$PATH"
# Tree-sitter
export PATH="/opt/homebrew/opt/tree-sitter@0.25/libexec/bin:$PATH"
# TeX
export PATH="/Library/TeX/texbin:$PATH"
# Custom bin
export PATH="/Users/madonnaprayer/.opencode/bin:$PATH"

# Homebrew
if [[ -f /opt/homebrew/bin/brew ]]; then
  eval "$(/opt/homebrew/bin/brew shellenv)"
fi

# =========================
# Oh My Zsh
# =========================
export ZSH="$HOME/.oh-my-zsh"
ZSH_THEME="powerlevel10k/powerlevel10k"

plugins=(
  git
  zsh-autosuggestions
  zsh-syntax-highlighting
)

source $ZSH/oh-my-zsh.sh

# =========================
# aliases
# =========================
export CLICOLOR=1
export CLICOLOR_FORCE=1
export LSCOLORS='cxfxcxdxbxegedabagacad'

alias u='cursor'  # Ensure `cursor` exists
alias c='code --reuse-window'
alias vim='nvim'
alias v='nvim'
alias vi='nvim'
alias ll='ls -la'
alias ls='CLICOLOR_FORCE=1 LSCOLORS=cxfxcxdxbxegedabagacad command ls -G'
alias gs='git status'
alias gd='git diff'
alias ..='cd ..'
alias reload='source ~/.zshrc'
alias ta='task add'
alias tn='task next'
alias tl='task list'
alias tt='taskwarrior-tui'

# =========================
# options
# =========================
ENABLE_CORRECTION="true"
CASE_SENSITIVE="false"

# =========================
# History
# =========================
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.zsh_history
setopt SHARE_HISTORY
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_ALL_DUPS

# =========================
# Completion Caching
# =========================
mkdir -p ~/.zsh/cache
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/cache

# =========================
# Language/Environment Managers
# =========================

# nvm (node)
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"

# gvm (go)
[[ -s "$HOME/.gvm/scripts/gvm" ]] && source "$HOME/.gvm/scripts/gvm"

# pyenv (python)
export PYENV_ROOT="$HOME/.pyenv"
command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"

# asdf
[ -f "$HOME/.asdf/asdf.sh" ] && . "$HOME/.asdf/asdf.sh"

# bun
export BUN_INSTALL="$HOME/.bun"
[ -s "$BUN_INSTALL/_bun" ] && source "$BUN_INSTALL/_bun"
export PATH="$BUN_INSTALL/bin:$PATH"

# ghcup (haskell)
[ -f "$HOME/.ghcup/env" ] && . "$HOME/.ghcup/env"

# pnpm
export PNPM_HOME="$HOME/Library/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac

# Conda
__conda_setup="$('/opt/miniconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    [ -f "/opt/miniconda3/etc/profile.d/conda.sh" ] && . "/opt/miniconda3/etc/profile.d/conda.sh" || \
        export PATH="/opt/miniconda3/bin:$PATH"
fi
unset __conda_setup

# =========================
# opam (ocaml)
# =========================
[ -r "$HOME/.opam/opam-init/init.zsh" ] && source "$HOME/.opam/opam-init/init.zsh" > /dev/null 2> /dev/null

# =========================
# powerlevel10k configuration
# =========================
[ -f ~/.p10k.zsh ] && source ~/.p10k.zsh
