return {
	"metalelf0/black-metal-theme-neovim",
	lazy = false,
	priority = 1000,
	config = function()
		require("black-metal").setup({
			theme = "khold",
			code_style = {
				comments = "italic",
				conditionals = "none",
				functions = "none",
				keywords = "none",
				headings = "bold", -- markdown headings
				operators = "none",
				keyword_return = "none",
				strings = "none",
				variables = "none",
			},
		})
		require("black-metal").load()
	end,
}

-- return {
-- 	"folke/tokyonight.nvim",
-- 	lazy = false,
-- 	priority = 1000,
-- 	config = function()
-- 		require("tokyonight").setup({
-- 			styles = {
-- 				comments = { italic = true },
-- 				keywords = { italic = false },
-- 				functions = { italic = false },
-- 				variables = { italic = false },
-- 			},
-- 		})
-- 		vim.cmd([[colorscheme tokyonight-night]])
-- 	end,
-- }

-- return {
-- 	"rose-pine/neovim",
-- 	name = "rose-pine",
-- 	config = function()
-- 		require("rose-pine").setup({
-- 			styles = {
-- 				bold = false,
-- 				italic = false,
-- 				transparency = false,
-- 			},
-- 			highlight_groups = {
-- 				Normal = { bg = "#000000" },
-- 				NormalNC = { bg = "#000000" },
-- 				NormalFloat = { bg = "#000000" },
-- 			},
-- 		})
-- 		vim.cmd("colorscheme rose-pine")
-- 	end,
-- }
