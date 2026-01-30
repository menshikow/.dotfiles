return {
	{
		"lewis6991/gitsigns.nvim",
		opts = {},
	},
	{
		"rebelot/kanagawa.nvim",
		name = "kanagawa",
		priority = 1000,
		config = function()
			require('kanagawa').setup({
				theme = "wave",
				transparent = false,
			})
			vim.cmd("colorscheme kanagawa")
		end,
	},
}
