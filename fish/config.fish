# =========================
# PATH and Homebrew
# =========================
set -gx PATH $HOME/.local/bin $PATH
set -gx PATH /opt/homebrew/opt/tree-sitter@0.25/libexec/bin $PATH
set -gx PATH /Library/TeX/texbin $PATH
set -gx PATH /Applications/Emacs.app/Contents/MacOS $PATH

if test -f /opt/homebrew/bin/brew
    eval (/opt/homebrew/bin/brew shellenv)
end

# =========================
# Tmux Auto-Start
# =========================
if status is-interactive
    and not set -q TMUX
    exec tmux new-session -A -s main
end

# =========================
# UI / Terminal Colors
# =========================
set -gx CLICOLOR 1
set -gx CLICOLOR_FORCE 1
set -gx LSCOLORS cxfxcxdxbxegedabagacad

# =========================
# Custom Greeting
# =========================
function fish_greeting
end

# =========================
# Colored Man Pages
# =========================
set -x LESS_TERMCAP_mb (set_color -o red)
set -x LESS_TERMCAP_md (set_color -o blue)
set -x LESS_TERMCAP_me (set_color normal)
set -x LESS_TERMCAP_se (set_color normal)
set -x LESS_TERMCAP_so (set_color -b 246)
set -x LESS_TERMCAP_ue (set_color normal)
set -x LESS_TERMCAP_us (set_color -u 146)

# =========================
# Abbreviations
# =========================
# General
abbr -a ls "eza -l -a"
abbr -a e "eza -l -a"
abbr -a vim "nvim"
abbr -a u "cursor"
abbr -a c "code --reuse-window"
abbr -a .. "cd .."
abbr -a reload "source ~/.config/fish/config.fish"

# Git
abbr -a g git
abbr -a gs "git status"
abbr -a gd "git diff"
abbr -a gc "git checkout"
abbr -a ga "git add -p"
abbr -a gah "git stash; and git pull --rebase; and git stash pop"

# =========================
# Dev Tools Setup
# =========================

# Pyenv
set -gx PYENV_ROOT $HOME/.pyenv
set -gx PATH $PYENV_ROOT/bin $PATH
if type -q pyenv
    pyenv init - | source
end

# ASDF
if test -f $HOME/.asdf/asdf.fish
    source $HOME/.asdf/asdf.fish
end

# Bun
set -gx BUN_INSTALL $HOME/.bun
set -gx PATH $BUN_INSTALL/bin $PATH

# GHCup
fish_add_path $HOME/.cabal/bin $HOME/.ghcup/bin

# PNPM
set -gx PNPM_HOME $HOME/Library/pnpm
if not contains $PNPM_HOME $PATH
    set -gx PATH $PNPM_HOME $PATH
end

# Opam
if test -f $HOME/.opam/opam-init/init.fish
    source $HOME/.opam/opam-init/init.fish
end

# =========================
# Custom Functions & Binds
# =========================

# Type 'd' to move up to the nearest parent directory that is a git repository
function d
    while test $PWD != "/"
        if test -d .git
            break
        end
        cd ..
    end
end

function fish_user_key_bindings
    # Press Ctrl+Z to suspend a job, and Ctrl+Z again to bring it back to the foreground
    bind \cz 'fg 2>/dev/null; commandline -f repaint'
    
    if functions -q fzf_key_bindings
        fzf_key_bindings
    end
end

# =========================
# Prompt
# =========================

function fish_prompt
    set -l time (date "+%H:%M")
    set -l host (hostname -s)
    set -l dir (prompt_pwd)

    # Git branch
    set -l branch (git branch --show-current 2>/dev/null)

    set_color 565f89
    echo -n "[$time] "

    set_color bb9af7
    echo -n "$host"

    set_color a9b1d6
    echo -n ":"

    set_color e0af68
    echo -n "$dir"

    if test -n "$branch"
        set_color 7dcfff
        echo -n " ($branch)"
    end

    set_color f7768e
    echo -n " ;; "

    set_color normal
end

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
if test -f /opt/miniconda3/bin/conda
    eval /opt/miniconda3/bin/conda "shell.fish" "hook" $argv | source
else
    if test -f "/opt/miniconda3/etc/fish/conf.d/conda.fish"
        . "/opt/miniconda3/etc/fish/conf.d/conda.fish"
    else
        set -x PATH "/opt/miniconda3/bin" $PATH
    end
end
# <<< conda initialize <<<

