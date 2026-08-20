return {
	"rose-pine/neovim",
	name = "rose-pine",
	lazy = false,
	priority = 1000,
	config = function()
		require("rose-pine").setup({
			highlight_groups = {
				Normal = { bg = "#000000" },
				NormalNC = { bg = "#000000" },
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

-- return {
-- 	"folke/tokyonight.nvim",
-- 	name = "tokyonight",
-- 	lazy = false,
-- 	priority = 1000,
-- 	opts = {
-- 		style = "night",
-- 		styles = {
-- 			comments = { italic = false },
-- 			keywords = { italic = false },
-- 			functions = { italic = false },
-- 			variables = { italic = false },
-- 			sidebars = { italic = false },
-- 			floats = { italic = false },
-- 		},
-- 		on_highlights = function(hl, c)
-- 			hl.Normal = { bg = "#000000" }
-- 			hl.NormalNC = { bg = "#000000" }
-- 			hl.NormalFloat = { bg = "#000000" }
-- 			hl.FloatBorder = { bg = "#000000" }
-- 			hl.SignColumn = { bg = "#000000" }
-- 			hl.LineNr = { bg = "#000000" }
-- 			hl.CursorLineNr = { bg = "#000000" }
-- 			hl.StatusLine = { bg = "#000000" }
-- 			hl.StatusLineNC = { bg = "#000000" }
-- 			hl.TabLine = { bg = "#000000" }
-- 			hl.TabLineFill = { bg = "#000000" }
-- 			hl.TabLineSel = { bg = "#000000" }
-- 			hl.Pmenu = { bg = "#000000" }
-- 			hl.PmenuSel = { bg = "#000000" }
-- 			hl.WinSeparator = { bg = "#000000" }
-- 		end,
-- 	},
-- 	config = function(_, opts)
-- 		require("tokyonight").setup(opts)
-- 		vim.cmd.colorscheme("tokyonight-night")
-- 	end,
-- }
