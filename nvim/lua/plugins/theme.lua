return {
	"rose-pine/neovim",
	name = "rose-pine",
	config = function()
		require("rose-pine").setup({
			styles = {
				bold = false,
				italic = false,
				transparency = false,
			},
			-- highlight_groups = {
			-- 	Normal = { bg = "#000000" },
			-- 	NormalNC = { bg = "#000000" },
			--
			-- 	Cursor = { fg = "NONE", bg = "NONE" },
			-- },
		})
		vim.cmd("colorscheme rose-pine")
	end,
}
