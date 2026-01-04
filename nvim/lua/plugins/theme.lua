return {
	"rose-pine/neovim",
	name = "rose-pine",
	priority = 1000,
	config = function()
		require("rose-pine").setup({
			variant = "main",
			styles = { italic = false, transparency = false, bold = false },
			highlight_groups = {
				Normal = { bg = "#000000" },
				NormalFloat = { bg = "#000000" },
				SignColumn = { bg = "#000000" },
				EndOfBuffer = { fg = "#000000" },
			},
		})

		vim.cmd.colorscheme("rose-pine")
	end,
}
