return {
	"rose-pine/neovim",
	name = "rose-pine",
	lazy = false,
	priority = 1000,
	config = function()
		require("rose-pine").setup({
			highlight_groups = {
				Normal = { bg = "#000000" },
				NormalFloat = { bg = "#000000" },
				FloatBorder = { bg = "#000000" },
				SignColumn = { bg = "#000000" },
				LineNr = { bg = "#000000" },
				CursorLineNr = { bg = "#000000" },
				StatusLine = { bg = "#000000" },
				StatusLineNC = { bg = "#000000" },
				TabLine = { bg = "#000000" },
				TabLineFill = { bg = "#000000" },
				TabLineSel = { bg = "#000000" },
				Pmenu = { bg = "#000000" },
				PmenuSel = { bg = "#000000" },
				WinSeparator = { bg = "#000000" },
			},

			styles = {
				italic = false,
			},
		})

		vim.cmd.colorscheme("rose-pine")

		vim.api.nvim_set_hl(0, "Cursor", { bg = "#ffffff", fg = "#000000" })
		vim.api.nvim_set_hl(0, "iCursor", { bg = "#ffffff", fg = "#000000" })
		vim.api.nvim_set_hl(0, "lCursor", { bg = "#ffffff", fg = "#000000" })
	end,
}
