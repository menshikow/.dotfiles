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
	"p00f/alabaster.nvim",
	lazy = false,
	priority = 1000,
	config = function()
		vim.cmd.colorscheme("alabaster")
	end,
}
