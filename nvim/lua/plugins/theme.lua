return {
	{
		"nvim-lualine/lualine.nvim",
		opts = {
			options = {
				theme = "auto",
			},
		},
	},
	{
		"tjdevries/gruvbuddy.nvim",
		lazy = false,
		priority = 1000,
		dependencies = {
			"tjdevries/colorbuddy.nvim",
		},
		config = function()
			vim.cmd.colorscheme("gruvbuddy")
		end,
	},
}
