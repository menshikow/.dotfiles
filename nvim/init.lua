require("config.lazy")
require("config.settings")
require("config.keymaps")

vim.env.PATH = vim.env.PATH .. ":/opt/homebrew/bin"

-- node fixes
vim.g.node_host_prog = "/Users/madonnaprayer/.nvm/versions/node/v22.20.0/bin/node"

local node_bin = "/Users/madonnaprayer/.nvm/versions/node/v22.20.0/bin"
if not string.find(vim.env.PATH, node_bin) then
	vim.env.PATH = node_bin .. ":" .. vim.env.PATH
end
