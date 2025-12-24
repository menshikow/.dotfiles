-- return {
-- 	{
-- 		"mcchrish/zenbones.nvim",
-- 		dependencies = "rktjmp/lush.nvim",
-- 		lazy = false,
-- 		priority = 1000,
-- 		config = function()
-- 			vim.opt.termguicolors = true
--
-- 			vim.g.zenbones_darken_comments = 45
-- 			vim.g.zenbones_lighten_noncurrent_window = flase
-- 			vim.g.zenbones_transparent_background = false
--
-- 			vim.cmd.colorscheme("zenbones")
-- 		end,
-- 	},
-- }

return {
	"folke/tokyonight.nvim",
	lazy = false,
	priority = 1000,
	opts = {
		styles = {
			comments = { italic = true },
			keywords = { italic = false },
			functions = { italic = false },
			variables = { italic = false },
			sidebars = "dark",
			floats = "dark",
		},
	},
	config = function(_, opts)
		require("tokyonight").setup(opts)
		vim.cmd([[colorscheme tokyonight-night]])
	end,
}
