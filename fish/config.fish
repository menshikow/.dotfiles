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
abbr -a l "ls -l -a"
abbr -a ls "ls -l -a"
abbr -a vim "vim"
abbr -a e "eza -l -a"
abbr -a c "code --reuse-window"
abbr -a e "eza -l -a"
abbr -a .. "cd .."
abbr -a ... "cd ../.."
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

# Rust / Cargo
fish_add_path $HOME/.cargo/bin

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

