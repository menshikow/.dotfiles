-- return {
-- 	"phha/zenburn.nvim",
-- 	lazy = false,
-- 	priority = 1000,
-- 	config = function()
-- 		vim.cmd.colorscheme("zenburn")
-- 		vim.api.nvim_set_hl(0, "Normal", {
-- 			bg = "#000000",
-- 		})
-- 	end,
-- }

return {
	"blazkowolf/gruber-darker.nvim",
	lazy = false,
	priority = 1000,

	config = function()
		require("gruber-darker").setup({
			italic = {
				strings = false,
				comments = false,
				operators = false,
				folds = false,
			},
		})

		vim.cmd.colorscheme("gruber-darker")
	end,
}
