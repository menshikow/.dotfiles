# fish configuration

# Shell syntax colors
if status is-interactive
    fish_config theme choose Nord
    set -g fish_color_autosuggestion 7b88a1
    set -g fish_color_comment 7b88a1 --italics
end

# Prompt
function fish_prompt
    set_color normal
    printf '%s@%s:' "$USER" (prompt_hostname)
    set_color 579692
    printf '%s' (prompt_pwd --dir-length=0)
    set_color normal
    printf '%s ' '$'
end

function fish_mode_prompt
end

# Greeting
function fish_greeting
	set_color 7b88a1
	echo (hostname)" — "(uname -sr)
	echo (uptime -p 2>/dev/null; or uptime)
	set_color normal
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


# BEGIN opam configuration
# This is useful if you're using opam as it adds:
#   - the correct directories to the PATH
#   - auto-completion for the opam binary
# This section can be safely removed at any time if needed.
test -r '/Users/madonnaprayer/.opam/opam-init/init.fish' && source '/Users/madonnaprayer/.opam/opam-init/init.fish' > /dev/null 2> /dev/null; or true
# END opam configuration
