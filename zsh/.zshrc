# Enable Powerlevel10k instant prompt. Should stay close to the top.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Homebrew
if [[ -f /opt/homebrew/bin/brew ]]; then
    eval "$(/opt/homebrew/bin/brew shellenv)"
fi

# Treesitter
export PATH="/opt/homebrew/opt/tree-sitter@0.25/libexec/bin:$PATH"

# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"
ZSH_THEME="robbyrussell"

# Enable plugins
# (Hinweis: Wenn du sie hier auflistest, kümmert sich OMZ meistens um das Sourcing)
plugins=(
  git
  zsh-autosuggestions
  zsh-syntax-highlighting
)

# Source Oh My Zsh
source $ZSH/oh-my-zsh.sh

# Manuelles Sourcing (falls nötig, lasse ich es drin, aber eigentlich macht das OMZ oben schon)
[ -f "$ZSH/custom/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh" ] && source "$ZSH/custom/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
[ -f "$ZSH/custom/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh" ] && source "$ZSH/custom/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh"

# Aliases
alias u='cursor'
alias c='code --reuse-window'
alias vim='nvim'
alias vi='nvim'
alias ll='ls -la'
alias gs='git status'
alias gd='git diff'
alias ..='cd ..'
alias reload='source ~/.zshrc' # Hilfreich zum Neuladen der Config

# Enable auto-correction of mistyped commands
ENABLE_CORRECTION="true"

# Case-insensitive globbing and completion
CASE_SENSITIVE="false"
HIST_IGNORE_DUPS="true"
HIST_IGNORE_ALL_DUPS="true"

# History config
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.zsh_history
setopt SHARE_HISTORY

# Enable command auto-completion
autoload -Uz compinit
compinit

# Prompt speed optimization
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/cache

# Load Powerlevel10k config
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

# ==============================================
# LANGUAGE ENVIRONMENTS
# ==============================================

# NVM (Node)
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"

# GVM (Go) - Korrigiert für macOS, falls installiert
[[ -s "$HOME/.gvm/scripts/gvm" ]] && source "$HOME/.gvm/scripts/gvm"

# TeX (MacTeX)
export PATH="/Library/TeX/texbin:$PATH"

# Local bins
[ -f "$HOME/.local/bin/env" ] && . "$HOME/.local/bin/env"
export PATH="$HOME/.local/bin:$PATH" # Sicherheitshalber

# Pyenv (Python)
export PYENV_ROOT="$HOME/.pyenv"
command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"

# Bun
[ -s "/Users/madonnaprayer/.bun/_bun" ] && source "/Users/madonnaprayer/.bun/_bun"
export BUN_INSTALL="$HOME/.bun"
export PATH="$BUN_INSTALL/bin:$PATH"

# GHCup (Haskell)
[ -f "/Users/madonnaprayer/.ghcup/env" ] && . "/Users/madonnaprayer/.ghcup/env"

# ASDF
[ -f "$HOME/.asdf/asdf.sh" ] && . "$HOME/.asdf/asdf.sh"

# pnpm
export PNPM_HOME="/Users/madonnaprayer/Library/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac

# Conda
__conda_setup="$('/opt/miniconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/opt/miniconda3/etc/profile.d/conda.sh" ]; then
        . "/opt/miniconda3/etc/profile.d/conda.sh"
    else
        export PATH="/opt/miniconda3/bin:$PATH"
    fi
fi
unset __conda_setup
