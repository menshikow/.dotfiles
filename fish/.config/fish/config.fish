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
