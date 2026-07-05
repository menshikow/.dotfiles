vim.env.PATH = vim.env.PATH .. ":/opt/homebrew/bin"

-- Workaround for Neovim 0.12.x treesitter `range` nil error
-- when processing `conceal_lines` in markdown LSP floating previews.
local get_range = vim.treesitter.get_range
vim.treesitter.get_range = function(node, source, metadata)
	if node then
		return get_range(node, source, metadata)
	end
	return { 0, 0, 0, 0 }
end

require("config.lazy")
require("config.settings")
require("config.keymaps")

vim.cmd.colorscheme("void")

local node_bin = vim.fn.trim(vim.fn.system("which node 2>/dev/null"))
if node_bin ~= "" then
	local node_dir = vim.fn.fnamemodify(node_bin, ":h")
	vim.g.node_host_prog = node_bin
	if not string.find(vim.env.PATH, node_dir) then
		vim.env.PATH = node_dir .. ":" .. vim.env.PATH
	end
end
