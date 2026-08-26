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
-- 	"Mofiqul/vscode.nvim",
-- 	name = "vscode",
-- 	lazy = false,
-- 	priority = 1000,
-- 	config = function()
-- 		local c = require("vscode.colors").get_colors()
-- 		require("vscode").setup({
-- 			style = "dark",
-- 			transparent = false,
-- 			italic_comments = false,
-- 			color_overrides = {
-- 				vscBack = "#000000",
-- 				vscTabCurrent = "#000000",
-- 				vscTabOther = "#000000",
-- 				vscTabOutside = "#000000",
-- 				vscLeftDark = "#000000",
-- 				vscLeftMid = "#000000",
-- 				vscPopupBack = "#000000",
-- 				vscCursorDarkDark = "#000000",
-- 				vscFoldBackground = "#000000",
-- 				vscContext = "#000000",
-- 			},
-- 		})
-- 		vim.cmd.colorscheme("vscode")
-- 	end,
-- }

-- return {
-- 	"ellisonleao/gruvbox.nvim",
-- 	lazy = false,
-- 	priority = 1000,
-- 	config = function()
-- 		local gruvbox = require("gruvbox")
-- 		local colors = gruvbox.palette -- exposes the real hex values (bg0, gray, red, yellow, etc.)
--
-- 		gruvbox.setup({
-- 			contrast = "hard",
-- 			transparent_mode = false,
-- 			italic = {
-- 				strings = false,
-- 				comments = false,
-- 				operators = false,
-- 				folds = false,
-- 			},
-- 			overrides = {
-- 				Normal = { bg = "#0B0B0C" },
-- 				NormalNC = { bg = "#0B0B0C" },
-- 				NormalFloat = { bg = "#0B0B0C" },
-- 				FloatBorder = { bg = "#0B0B0C" },
-- 				SignColumn = { bg = "#0B0B0C" },
-- 				LineNr = { bg = "#0B0B0C" },
-- 				CursorLineNr = { bg = "#0B0B0C" },
-- 				StatusLine = { bg = "#0B0B0C" },
-- 				StatusLineNC = { bg = "#0B0B0C" },
-- 				TabLine = { bg = "#0B0B0C" },
-- 				TabLineFill = { bg = "#0B0B0C" },
-- 				TabLineSel = { bg = "#0B0B0C" },
-- 				Pmenu = { bg = "#0B0B0C" },
-- 				PmenuSel = { bg = "#0B0B0C" },
-- 				WinSeparator = { bg = "#0B0B0C" },
--
-- 				DiagnosticError = { fg = colors.neutral_red },
-- 				DiagnosticWarn = { fg = colors.neutral_yellow },
-- 				DiagnosticInfo = { fg = colors.gray },
-- 				DiagnosticHint = { fg = colors.dark4 },
--
-- 				DiagnosticVirtualTextError = { fg = colors.neutral_red, italic = false },
-- 				DiagnosticVirtualTextWarn = { fg = colors.neutral_yellow, italic = false },
-- 				DiagnosticVirtualTextInfo = { fg = colors.gray, italic = true },
-- 				DiagnosticVirtualTextHint = { fg = colors.dark4, italic = true },
--
-- 				DiagnosticSignError = { fg = colors.neutral_red },
-- 				DiagnosticSignWarn = { fg = colors.neutral_yellow },
-- 				DiagnosticSignInfo = { fg = colors.gray },
-- 				DiagnosticSignHint = { fg = colors.dark4 },
--
-- 				DiagnosticUnderlineError = { undercurl = true, sp = colors.neutral_red },
-- 				DiagnosticUnderlineWarn = { undercurl = true, sp = colors.neutral_yellow },
-- 			},
-- 		})
-- 		vim.cmd.colorscheme("gruvbox")
-- 	end,
-- }

-- return {
-- 	"folke/tokyonight.nvim",
-- 	lazy = false,
-- 	priority = 1000,
-- 	config = function()
-- 		require("tokyonight").setup({
-- 			style = "night",
-- 			transparent = false,
-- 			styles = {
-- 				comments = { italic = false },
-- 				keywords = { italic = false },
-- 			},
-- 			on_highlights = function(hl)
-- 				local bg = "#0B0B0C"
-- 				hl.Normal = { bg = bg }
-- 				hl.NormalNC = { bg = bg }
-- 				hl.NormalFloat = { bg = bg }
-- 				hl.FloatBorder = { bg = bg }
-- 				hl.SignColumn = { bg = bg }
-- 				hl.LineNr = { bg = bg }
-- 				hl.CursorLineNr = { bg = bg }
-- 				hl.StatusLine = { bg = bg }
-- 				hl.StatusLineNC = { bg = bg }
-- 				hl.TabLine = { bg = bg }
-- 				hl.TabLineFill = { bg = bg }
-- 				hl.TabLineSel = { bg = bg }
-- 				hl.Pmenu = { bg = bg }
-- 				hl.PmenuSel = { bg = bg }
-- 				hl.WinSeparator = { bg = bg }
-- 			end,
-- 		})
-- 		vim.cmd.colorscheme("tokyonight")
-- 	end,
-- }

-- return {
-- 	"shaunsingh/nord.nvim",
-- 	lazy = false,
-- 	priority = 1000,
-- 	config = function()
-- 		vim.g.nord_contrast = true
-- 		vim.g.nord_borders = false
-- 		vim.g.nord_disable_background = false
-- 		vim.g.nord_italic = false
-- 		vim.g.nord_uniform_diff_background = true
-- 		vim.g.nord_bold = true
--
-- 		vim.cmd.colorscheme("nord")
-- 	end,
-- }
