# ── Lambda Theme ──────────────────────────────────────────────

_color() { printf '\[\033[%sm\]' "$1"; }
GREEN=$(_color 32)
BLUE=$(_color 34)
YELLOW=$(_color 33)
RED=$(_color 31)
CYAN=$(_color 36)
RESET=$(_color 0)
BOLD=$(_color 1)

__git_branch() {
  local b
  b=$(git symbolic-ref --short HEAD 2>/dev/null)
  [ -n "$b" ] && printf " (%s)" "$b"
}

__prompt() {
  local exit=$?
  local symbol=" λ "
  local dir="${PWD/#$HOME/\~}"
  local branch="$(__git_branch)"
  local prefix=""
  local suffix=""
  if [ "$exit" -ne 0 ]; then
    prefix="${RED}"
    suffix=" ${RED}✗${RESET}"
  else
    prefix="${GREEN}"
    suffix=""
  fi
  PS1="${prefix}${symbol}${RESET}${BLUE}${dir}${RESET}${YELLOW}${branch}${RESET} →${suffix} "
}

PROMPT_COMMAND=__prompt

# ── Shell Options ─────────────────────────────────────────────

# Only run shopt in bash (not zsh) and skip unsupported options
if command -v shopt &>/dev/null; then
    shopt -s autocd cdable_vars checkwinsize cmdhist 2>/dev/null || true
    shopt -s histappend histreedit histverify 2>/dev/null || true
    shopt -s no_empty_cmd_completion 2>/dev/null || true
fi

HISTSIZE=100000
HISTFILESIZE=200000
HISTCONTROL=ignoreboth:erasedups

# ── PATH & Environment ────────────────────────────────────────

addpath() { case ":${PATH:=$1}:" in *:"$1":*) ;; *) PATH="$1:$PATH";; esac; }

addpath "$HOME/.local/bin"
addpath "$HOME/.cargo/bin"
addpath "$HOME/.ghcup/bin"
addpath "$HOME/.cabal/bin"
addpath "/opt/homebrew/bin"

# OCaml
[ -f "$HOME/.opam/opam-init/init.sh" ] && . "$HOME/.opam/opam-init/init.sh" 2>/dev/null

# Haskell
[ -f "$HOME/.ghcup/env" ] && . "$HOME/.ghcup/env"

# Conda
__conda_setup="$('/opt/miniconda3/bin/conda' 'shell.bash' 'hook' 2>/dev/null)"
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

# ── Aliases ───────────────────────────────────────────────────

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

# ── Language-Specific Helpers ─────────────────────────────────

ocaml_repl() { rlwrap ocaml -init <(echo '#use "topfind";;'); }
cabal_repl() { cabal repl --repl-options=-fno-code; }
hs_repl() { stack ghci --ghci-options=-fno-code; }

make_targets() { grep -E '^[a-zA-Z_-]+:' Makefile 2>/dev/null | cut -d: -f1 | sort; }
