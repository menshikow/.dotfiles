# fish configuration

# Prompt
function fish_prompt
	set_color 665c54 # base03 - muted, for timestamp
	echo -n "["(date "+%H:%M")"] "
	set_color 83a598 # base0D - blue, for hostname
	echo -n (command -q hostname; and hostname; or hostnamectl hostname)
	if [ $PWD != $HOME ]
		set_color 665c54 # base03
		echo -n ':'
		set_color fabd2f # base0A - yellow, for dir
		echo -n (basename $PWD)
	end
	set_color b8bb26 # base0B - green, for git status
	printf '%s ' (__fish_git_prompt)
	set_color fb4934 # base08 - red, for separator
	echo -n '| '
	set_color normal
end

set __fish_git_prompt_showuntrackedfiles 'yes'
set __fish_git_prompt_showdirtystate 'yes'
set __fish_git_prompt_showstashstate ''
set __fish_git_prompt_showupstream 'none'

# Greeting
function fish_greeting
	set_color brblack
	echo (hostname)" — "(uname -sr)
	echo (uptime -p 2>/dev/null; or uptime)
end

# Colored Man Pages
set -x LESS_TERMCAP_mb (set_color -o red)
set -x LESS_TERMCAP_md (set_color -o blue)
set -x LESS_TERMCAP_me (set_color normal)
set -x LESS_TERMCAP_se (set_color normal)
set -x LESS_TERMCAP_so (set_color -b 246)
set -x LESS_TERMCAP_ue (set_color normal)
set -x LESS_TERMCAP_us (set_color -u 146)

# fzf (only if fd is installed)
if command -v fd > /dev/null
	set -x FZF_DEFAULT_COMMAND 'fd --type file --follow'
	set -x FZF_CTRL_T_COMMAND 'fd --type file --follow'
end
set -x FZF_DEFAULT_OPTS '--height 20%'

# Abbreviations

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

# Functions

function fish_user_key_bindings
    fish_vi_key_bindings
    bind -M insert \cf forward-char
    bind -M normal \cf forward-char
end

# Jump to the root of the current git repo
function d
	while test $PWD != "/"
		if test -d .git
			break
		end
		cd ..
	end
end
