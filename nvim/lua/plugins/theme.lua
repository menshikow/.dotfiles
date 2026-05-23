-- return {
-- 	{
-- 		"tjdevries/gruvbuddy.nvim",
-- 		lazy = false,
-- 		priority = 1000,
-- 		dependencies = {
-- 			"tjdevries/colorbuddy.nvim",
-- 		},
--
-- 		config = function()
-- 			vim.cmd.colorscheme("gruvbuddy")
-- 		end,
-- 	},
-- }
--
--

return {
	"menshikow/vim-mono.nvim",
	lazy = false,
	priority = 1000,

	config = function()
		vim.cmd.colorscheme("vim-mono")
	end,
}
