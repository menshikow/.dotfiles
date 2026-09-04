# =========================
# Prompt
# =========================
function fish_prompt
	set_color brblack
	echo -n "["(date "+%H:%M")"] "
	set_color blue
	echo -n (command -q hostname; and hostname; or hostnamectl hostname)
	if [ $PWD != $HOME ]
		set_color brblack
		echo -n ':'
		set_color yellow
		echo -n (basename $PWD)
	end
	set_color green
	printf '%s ' (__fish_git_prompt)
	set_color red
	echo -n '| '
	set_color normal
end

set __fish_git_prompt_showuntrackedfiles 'yes'
set __fish_git_prompt_showdirtystate 'yes'
set __fish_git_prompt_showstashstate ''
set __fish_git_prompt_showupstream 'none'

# =========================
# Greeting (lightweight)
# =========================
function fish_greeting
	set_color brblack
	echo (command -q hostname; and hostname; or hostnamectl hostname)" — "(uname -sr)
	echo (uptime -p 2>/dev/null; or uptime)
	set_color normal
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
# fzf (only if fd is installed)
# =========================
if command -v fd > /dev/null
	set -x FZF_DEFAULT_COMMAND 'fd --type file --follow'
	set -x FZF_CTRL_T_COMMAND 'fd --type file --follow'
end
set -x FZF_DEFAULT_OPTS '--height 20%'

# =========================
# Abbreviations
# =========================
# General
abbr -a l "ls -l -a"
abbr -a ls "ls -l -a"
abbr -a vim "vim"
abbr -a c "code --reuse-window"
abbr -a .. "cd .."
abbr -a ... "cd ../.."
abbr -a reload "source ~/.config/fish/config.fish"

if command -v eza > /dev/null
	abbr -a e "eza -l -a"
else
	abbr -a e "ls -l -a"
end

# Git
abbr -a g git
abbr -a gs "git status"
abbr -a gd "git diff"
abbr -a gc "git checkout"
abbr -a ga "git add -p"
abbr -a gah "git stash; and git pull --rebase; and git stash pop"

# =========================
# Functions
# =========================
# Jump to the root of the current git repo
function d
	while test $PWD != "/"
		if test -d .git
			break
		end
		cd ..
	end
end

# =========================
# opam configuration
# =========================
test -r '/home/adria/.opam/opam-init/init.fish' && source '/home/adria/.opam/opam-init/init.fish' > /dev/null 2> /dev/null; or true
