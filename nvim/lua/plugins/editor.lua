return {
	{
		"lewis6991/gitsigns.nvim",
		opts = {},
	},

	-- {
	-- 	"rcarriga/nvim-notify",
	-- 	config = function()
	-- 		local notify = require("notify")
	--
	-- 		notify.setup({
	-- 			stages = "static",
	-- 			timeout = 2000,
	-- 			render = "minimal",
	-- 			background_colour = "#000000",
	-- 			fps = 60,
	-- 			top_down = false,
	-- 		})
	--
	-- 		vim.notify = notify
	-- 	end,
	-- },
	{
		"mg979/vim-visual-multi",
		branch = "master",
		init = function()
			pcall(vim.keymap.del, "n", "<C-n>")
			pcall(vim.keymap.del, "x", "<C-n>")

			vim.g.VM_maps = {
				["Find Under"] = "<C-n>",
				["Find Subword Under"] = "<C-n>",
			}
		end,
	},
}
