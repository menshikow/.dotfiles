if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="macovsky"

# enable plugins
plugins=(
  git
  zsh-autosuggestions
  zsh-syntax-highlighting
)

# source Oh My Zsh
source $ZSH/oh-my-zsh.sh

# enable syntax highlighting (required to be after zsh path)
source $ZSH/custom/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# enable autosuggestions (recommended after sourcing oh-my-zsh)
source $ZSH/custom/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh

# aliases
alias c='code --reuse-window'
alias vim='nvim'
alias ll='ls -la'
alias gs='git status'
alias gd='git diff'
alias ..='cd ..'

# enable auto-correction of mistyped commands
ENABLE_CORRECTION="true"

# case-insensitive globbing and completion
CASE_SENSITIVE="false"
HIST_IGNORE_DUPS="true"
HIST_IGNORE_ALL_DUPS="true"

# history config
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.zsh_history

# share history across all sessions
setopt SHARE_HISTORY

# enable command auto-completion
autoload -Uz compinit
compinit

# prompt speed optimization (optional)
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/cache

# load Powerlevel10k instant prompt (boosts speed)
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" 
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"

[[ -s "/home/mda/.gvm/scripts/gvm" ]] && source "/home/mda/.gvm/scripts/gvm"

export PATH=/Library/TeX/texbin:$PATH
export PATH="/Library/TeX/texbin:$PATH"

. "$HOME/.local/bin/env"

# pyenv
export PYENV_ROOT="$HOME/.pyenv"
command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"

# bun completions
[ -s "/Users/madonnaprayer/.bun/_bun" ] && source "/Users/madonnaprayer/.bun/_bun"

# bun
export BUN_INSTALL="$HOME/.bun"
export PATH="$BUN_INSTALL/bin:$PATH"

[ -f "/Users/madonnaprayer/.ghcup/env" ] && . "/Users/madonnaprayer/.ghcup/env" # ghcup-env
