require("config.lazy")
require("config.settings")
require("config.keymaps")

-- Static colorscheme: hardcoded hex values, no programmatic generation
-- (replaces colorbuddy/gruvbuddy which was slow due to runtime color computation)
vim.cmd.colorscheme("gruvbuddy-static")

vim.env.PATH = vim.env.PATH .. ":/opt/homebrew/bin"

local node_bin = vim.fn.trim(vim.fn.system("which node 2>/dev/null"))
if node_bin ~= "" then
	local node_dir = vim.fn.fnamemodify(node_bin, ":h")
	vim.g.node_host_prog = node_bin
	if not string.find(vim.env.PATH, node_dir) then
		vim.env.PATH = node_dir .. ":" .. vim.env.PATH
	end
end
