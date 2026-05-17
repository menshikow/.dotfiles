return {
	{
		"lewis6991/gitsigns.nvim",
		opts = {},
	},
	{
		"rcarriga/nvim-notify",
		config = function()
			local notify = require("notify")

			notify.setup({
				stages = "fade",
				timeout = 2000,
				render = "minimal",
				background_colour = "#000000",
				fps = 60,
				top_down = false,
			})

			vim.notify = notify
		end,
	},
}
