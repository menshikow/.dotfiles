# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="powerlevel10k/powerlevel10k"

# Enable plugins
plugins=(
  git
  zsh-autosuggestions
  zsh-syntax-highlighting
)

# Source Oh My Zsh
source $ZSH/oh-my-zsh.sh

# Enable syntax highlighting (required to be after oh-my-zsh.sh)
source $ZSH/custom/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# Enable autosuggestions (recommended after sourcing oh-my-zsh)
source $ZSH/custom/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh

# Some useful aliases
alias vim='nvim'
alias ll='ls -la'
alias gs='git status'
alias gd='git diff'
alias ..='cd ..'

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

# Share history across all sessions
setopt SHARE_HISTORY

# Enable command auto-completion
autoload -Uz compinit
compinit

# Prompt speed optimization (optional)
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/cache

# Load Powerlevel10k instant prompt (optional, boosts startup time)
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

[[ -s "/home/mda/.gvm/scripts/gvm" ]] && source "/home/mda/.gvm/scripts/gvm"
