# ── Oh My Zsh ──────────────────────────────────────────────
if [ -d "$HOME/.oh-my-zsh" ]; then
  export ZSH="$HOME/.oh-my-zsh"
  ZSH_THEME="lambda"
  plugins=(git brew docker macos gh node npm pip python cargo)
  source "$ZSH/oh-my-zsh.sh"
fi

# ── Shell Options ──────────────────────────────────────────
setopt autocd cdablevars interactivecomments
setopt extendedhistory histignorealldups histignoredups
setopt histreduceblanks histverify histnostore
setopt incappendhistory sharehistory
setopt nomatch notify globdots

HISTSIZE=100000
SAVEHIST=200000
HISTFILE="$HOME/.zsh_history"

# ── PATH & Environment ─────────────────────────────────────
addpath() { case ":${PATH:=$1}:" in *:"$1":*) ;; *) PATH="$1:$PATH";; esac; }

addpath "$HOME/.local/bin"
addpath "$HOME/.cargo/bin"
addpath "$HOME/.ghcup/bin"
addpath "$HOME/.cabal/bin"
addpath "/opt/homebrew/bin"

# OCaml
[ -f "$HOME/.opam/opam-init/init.zsh" ] && . "$HOME/.opam/opam-init/init.zsh" 2>/dev/null

# Haskell
[ -f "$HOME/.ghcup/env" ] && . "$HOME/.ghcup/env"

# Conda
__conda_setup="$('/opt/miniconda3/bin/conda' 'shell.zsh' 'hook' 2>/dev/null)"
if [ $? -eq 0 ]; then
  eval "$__conda_setup"
else
  [ -f "/opt/miniconda3/etc/profile.d/conda.sh" ] && . "/opt/miniconda3/etc/profile.d/conda.sh"
  addpath "/opt/miniconda3/bin"
fi
unset __conda_setup

export EDITOR=nvim
export VISUAL=nvim
export PAGER=less

# ── Aliases ─────────────────────────────────────────────────
alias ls='ls -G'
alias ll='ls -lhG'
alias la='ls -lhaG'
alias lt='ls -lhtG'
alias lr='ls -lhtrG'

alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'

alias g='git'
alias gs='git status'
alias ga='git add'
alias gc='git commit'
alias gp='git push'
alias gl='git log --oneline --graph --decorate'

alias m='make'
alias mc='make clean'
alias mb='make build'
alias mr='make run'

alias c='cabal'
alias cs='cabal build'
alias cr='cabal run'
alias ct='cabal test'
alias cghci='cabal repl'

alias hs='stack'
alias hsg='stack ghci'
alias hsb='stack build'
alias hst='stack test'

alias oc='ocaml'
alias ocb='ocamlbuild'
alias ocd='ocamldebug'

alias cpp='g++ -std=c++17 -Wall -Wextra'
alias cc='gcc -std=c17 -Wall -Wextra'

alias cl='clang -Wall -Wextra'
alias clxx='clang++ -std=c++17 -Wall -Wextra'

# ── Language-Specific Helpers ───────────────────────────────
ocaml_repl() { rlwrap ocaml -init <(echo '#use "topfind";;'); }
cabal_repl() { cabal repl --repl-options=-fno-code; }
hs_repl() { stack ghci --ghci-options=-fno-code; }

make_targets() { grep -E '^[a-zA-Z_-]+:' Makefile 2>/dev/null | cut -d: -f1 | sort; }

# ── Starship fallback prompt (if no oh-my-zsh) ──────────────
if [ ! -d "$HOME/.oh-my-zsh" ]; then
  precmd() {
    local exit=$?
    local dir="${PWD/#$HOME/\~}"
    local branch="$(git symbolic-ref --short HEAD 2>/dev/null)"
    local symbol=" λ "
    local suffix=""
    if [ "$exit" -ne 0 ]; then
      suffix=" %F{red}✗%f"
    fi
    PROMPT="%F{green}${symbol}%f%F{blue}${dir}%f%F{yellow}${branch:+ ($branch)}%f →${suffix} "
  }
fi

# ── Auto-start tmux ──────────────────────────────────────────
if [[ -z "$TMUX" && -z "$VSCODE_INJECTION" && "$TERM_PROGRAM" != "vscode" ]]; then
  exec tmux new-session -A -s main
fi
