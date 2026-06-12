# =========================
# PATH and Homebrew
# =========================

export PATH="$HOME/.local/bin:$PATH"
export PATH="/opt/homebrew/opt/tree-sitter@0.25/libexec/bin:$PATH"
export PATH="/Library/TeX/texbin:$PATH"

if [[ -f /opt/homebrew/bin/brew ]]; then
  eval "$(/opt/homebrew/bin/brew shellenv)"
fi

export ZSH="$HOME/.oh-my-zsh"
# Disabled to use custom prompt
ZSH_THEME=""

plugins=(
  git
  zsh-autosuggestions
  zsh-syntax-highlighting
)

source "$ZSH/oh-my-zsh.sh"

# =========================
# Aliases
# =========================

export CLICOLOR=1
export CLICOLOR_FORCE=1
export LSCOLORS='cxfxcxdxbxegedabagacad'

alias e='eza -l -a'
alias ls='eza -l -a'
alias ll='eza -l -a'
alias vim='nvim'
alias u='cursor'
alias c='code --reuse-window'
alias gs='git status'
alias gd='git diff'
alias ..='cd ..'
alias reload='source ~/.zshrc'
alias ta='task add'
alias tn='task next'
alias tl='task list'
alias tt='taskwarrior-tui'

# =========================
# Shell Options
# =========================

setopt CORRECT
unsetopt CASE_GLOB

# =========================
# History
# =========================

HISTSIZE=10000
SAVEHIST=10000
HISTFILE="$HOME/.zsh_history"

setopt SHARE_HISTORY
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_ALL_DUPS

# =========================
# Completion Cache
# =========================

autoload -Uz compinit
compinit

mkdir -p ~/.zsh/cache

zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/cache

# =========================
# NVM (Node.js)
# =========================

export NVM_DIR="$HOME/.nvm"

[ -s "$NVM_DIR/nvm.sh" ] && source "$NVM_DIR/nvm.sh"
[ -s "$NVM_DIR/bash_completion" ] && source "$NVM_DIR/bash_completion"

# =========================
# GVM (Go)
# =========================

[[ -s "$HOME/.gvm/scripts/gvm" ]] && source "$HOME/.gvm/scripts/gvm"

# =========================
# Pyenv
# =========================

export PYENV_ROOT="$HOME/.pyenv"

if ! command -v pyenv >/dev/null; then
  export PATH="$PYENV_ROOT/bin:$PATH"
fi

eval "$(pyenv init -)"

# =========================
# ASDF
# =========================

[ -f "$HOME/.asdf/asdf.sh" ] && source "$HOME/.asdf/asdf.sh"

# =========================
# Bun
# =========================

export BUN_INSTALL="$HOME/.bun"

[ -s "$BUN_INSTALL/_bun" ] && source "$BUN_INSTALL/_bun"

export PATH="$BUN_INSTALL/bin:$PATH"

# =========================
# GHCup (Haskell)
# =========================

[ -f "$HOME/.ghcup/env" ] && source "$HOME/.ghcup/env"

# =========================
# PNPM
# =========================

export PNPM_HOME="$HOME/Library/pnpm"

case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac

# =========================
# Conda
# =========================

__conda_setup="$('/opt/miniconda3/bin/conda' 'shell.zsh' 'hook' 2>/dev/null)"

if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/opt/miniconda3/etc/profile.d/conda.sh" ]; then
        source "/opt/miniconda3/etc/profile.d/conda.sh"
    else
        export PATH="/opt/miniconda3/bin:$PATH"
    fi
fi

unset __conda_setup

# =========================
# Opam (OCaml)
# =========================

[ -r "$HOME/.opam/opam-init/init.zsh" ] && \
    source "$HOME/.opam/opam-init/init.zsh" >/dev/null 2>/dev/null

# =========================
# Extra PATH Entries
# =========================

export PATH="/Applications/Emacs.app/Contents/MacOS:$PATH"


# ==========================================
# Gruvbox Prompt
# ==========================================
autoload -Uz vcs_info
precmd() { vcs_info }
# Git branch format: space, (branch), reset
zstyle ':vcs_info:git:*' formats ' %F{#98971a}(%b)%f'

setopt PROMPT_SUBST
# Format: [HH:MM] hostname:dir (git) |
# Colors: Gray, Blue, Gray, Yellow, Green, Red
PROMPT='%F{#928374}[%D{%H:%M}] %F{#458588}%m%F{#928374}:%F{#d79921}%1~%f${vcs_info_msg_0_} %F{#cc241d}| %f'
