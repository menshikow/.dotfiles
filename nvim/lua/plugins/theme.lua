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

return {
	"RostislavArts/naysayer.nvim",
	lazy = false,
	priority = 1000,
	config = function()
		vim.cmd.colorscheme("naysayer")

		local hl = vim.api.nvim_set_hl
		hl(0, "Normal",       { fg = "#d0b892", bg = "#000000" })
		hl(0, "NormalFloat",  { bg = "#000000" })
		hl(0, "LineNr",       { fg = "#126367", bg = "#000000" })
		hl(0, "CursorLineNr", { fg = "#ffffff", bg = "#000000" })
		hl(0, "CursorLine",   { bg = "#0a0a0a" })
		hl(0, "ColorColumn",  { bg = "#0a0a0a" })
		hl(0, "SignColumn",   { bg = "#000000" })
	end,
}
