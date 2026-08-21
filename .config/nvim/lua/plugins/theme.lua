return {
	"shaunsingh/nord.nvim",
	lazy = false,
	priority = 1000,
	config = function()
		vim.g.nord_contrast = true
		vim.g.nord_borders = false
		vim.g.nord_disable_background = false
		vim.g.nord_italic = false
		vim.g.nord_bold = false
		vim.g.nord_uniform_diff_background = true
		vim.cmd.colorscheme("nord")

		-- Custom palette override
		local palette = {
			bg = "#282c34",
			fg = "#ffffff",
			cursor_bg = "#ffffff",
			cursor_fg = "#353a44",
			selection_bg = "#ffffff",
			selection_fg = "#282c34",

			black = "#1d1f21",
			red = "#cc6566",
			green = "#b6bd68",
			yellow = "#f0c674",
			blue = "#82a2be",
			magenta = "#b294bb",
			cyan = "#8abeb7",
			white = "#c4c8c6",
			bright_black = "#666666",
			bright_red = "#d54e53",
			bright_green = "#b9ca4b",
			bright_yellow = "#e7c547",
			bright_blue = "#7aa6da",
			bright_magenta = "#c397d8",
			bright_cyan = "#70c0b1",
			bright_white = "#eaeaea",
		}

		local hl = vim.api.nvim_set_hl

		-- Core UI
		hl(0, "Normal", { bg = palette.bg, fg = palette.fg })
		hl(0, "NormalNC", { bg = palette.bg, fg = palette.fg })
		hl(0, "NormalFloat", { bg = palette.bg, fg = palette.fg })
		hl(0, "FloatBorder", { bg = palette.bg, fg = palette.bright_black })
		hl(0, "SignColumn", { bg = palette.bg })
		hl(0, "LineNr", { bg = palette.bg, fg = palette.bright_black })
		hl(0, "CursorLineNr", { bg = palette.bg, fg = palette.fg })
		hl(0, "CursorLine", { bg = "#353a44" })
		hl(0, "CursorColumn", { bg = "#353a44" })
		hl(0, "ColorColumn", { bg = "#353a44" })
		hl(0, "StatusLine", { bg = palette.bg, fg = palette.fg })
		hl(0, "StatusLineNC", { bg = palette.bg, fg = palette.bright_black })
		hl(0, "TabLine", { bg = palette.bg, fg = palette.bright_black })
		hl(0, "TabLineFill", { bg = palette.bg })
		hl(0, "TabLineSel", { bg = palette.bg, fg = palette.fg })
		hl(0, "WinSeparator", { bg = palette.bg, fg = palette.bright_black })
		hl(0, "Pmenu", { bg = palette.bg, fg = palette.fg })
		hl(0, "PmenuSel", { bg = palette.selection_bg, fg = palette.selection_fg })
		hl(0, "PmenuSbar", { bg = palette.black })
		hl(0, "PmenuThumb", { bg = palette.bright_black })
		hl(0, "Folded", { bg = palette.bg, fg = palette.bright_black })
		hl(0, "FoldColumn", { bg = palette.bg, fg = palette.bright_black })
		hl(0, "VertSplit", { bg = palette.bg, fg = palette.bright_black })
		hl(0, "Visual", { bg = palette.selection_bg, fg = palette.selection_fg })
		hl(0, "VisualNOS", { bg = palette.selection_bg, fg = palette.selection_fg })
		hl(0, "Search", { bg = palette.yellow, fg = palette.black })
		hl(0, "IncSearch", { bg = palette.bright_yellow, fg = palette.black })
		hl(0, "CurSearch", { bg = palette.bright_yellow, fg = palette.black })
		hl(0, "MatchParen", { bg = palette.bright_black, fg = palette.yellow, bold = true })
		hl(0, "Whitespace", { fg = palette.bright_black })
		hl(0, "NonText", { fg = palette.bright_black })
		hl(0, "Conceal", { fg = palette.bright_black })
		hl(0, "Directory", { fg = palette.blue })
		hl(0, "Title", { fg = palette.green, bold = true })

		-- Cursor
		hl(0, "Cursor", { bg = palette.cursor_bg, fg = palette.cursor_fg })
		hl(0, "iCursor", { bg = palette.cursor_bg, fg = palette.cursor_fg })
		hl(0, "lCursor", { bg = palette.cursor_bg, fg = palette.cursor_fg })
		hl(0, "TermCursor", { bg = palette.cursor_bg, fg = palette.cursor_fg })
		hl(0, "TermCursorNC", { bg = palette.bright_black, fg = palette.fg })

		-- Syntax
		hl(0, "Comment", { fg = palette.bright_black, italic = false })
		hl(0, "Constant", { fg = palette.cyan })
		hl(0, "String", { fg = palette.green })
		hl(0, "Character", { fg = palette.green })
		hl(0, "Number", { fg = palette.yellow })
		hl(0, "Boolean", { fg = palette.yellow })
		hl(0, "Float", { fg = palette.yellow })
		hl(0, "Identifier", { fg = palette.red })
		hl(0, "Function", { fg = palette.blue })
		hl(0, "Statement", { fg = palette.magenta })
		hl(0, "Conditional", { fg = palette.magenta })
		hl(0, "Repeat", { fg = palette.magenta })
		hl(0, "Label", { fg = palette.magenta })
		hl(0, "Operator", { fg = palette.fg })
		hl(0, "Keyword", { fg = palette.red })
		hl(0, "Exception", { fg = palette.magenta })
		hl(0, "PreProc", { fg = palette.yellow })
		hl(0, "Include", { fg = palette.magenta })
		hl(0, "Define", { fg = palette.magenta })
		hl(0, "Macro", { fg = palette.magenta })
		hl(0, "PreCondit", { fg = palette.yellow })
		hl(0, "Type", { fg = palette.yellow })
		hl(0, "StorageClass", { fg = palette.yellow })
		hl(0, "Structure", { fg = palette.yellow })
		hl(0, "Typedef", { fg = palette.yellow })
		hl(0, "Special", { fg = palette.blue })
		hl(0, "SpecialChar", { fg = palette.fg })
		hl(0, "Tag", { fg = palette.fg })
		hl(0, "Delimiter", { fg = palette.fg })
		hl(0, "SpecialComment", { fg = palette.fg })
		hl(0, "Debug", { fg = palette.fg })
		hl(0, "Underlined", { fg = palette.fg, underline = true })
		hl(0, "Ignore", { fg = palette.bright_black })
		hl(0, "Error", { fg = palette.red, bg = palette.bg })
		hl(0, "Todo", { fg = palette.magenta, bold = true })

		-- Diagnostics
		hl(0, "DiagnosticError", { fg = palette.red })
		hl(0, "DiagnosticWarn", { fg = palette.yellow })
		hl(0, "DiagnosticInfo", { fg = palette.blue })
		hl(0, "DiagnosticHint", { fg = palette.cyan })
		hl(0, "DiagnosticVirtualTextError", { fg = palette.red, bg = palette.bg })
		hl(0, "DiagnosticVirtualTextWarn", { fg = palette.yellow, bg = palette.bg })
		hl(0, "DiagnosticVirtualTextInfo", { fg = palette.blue, bg = palette.bg })
		hl(0, "DiagnosticVirtualTextHint", { fg = palette.cyan, bg = palette.bg })
		hl(0, "DiagnosticUnderlineError", { undercurl = true, sp = palette.red })
		hl(0, "DiagnosticUnderlineWarn", { undercurl = true, sp = palette.yellow })
		hl(0, "DiagnosticUnderlineInfo", { undercurl = true, sp = palette.blue })
		hl(0, "DiagnosticUnderlineHint", { undercurl = true, sp = palette.cyan })

		-- Git
		hl(0, "diffAdded", { fg = palette.green })
		hl(0, "diffRemoved", { fg = palette.red })
		hl(0, "diffChanged", { fg = palette.yellow })
		hl(0, "DiffAdd", { bg = "#1d3b2e" })
		hl(0, "DiffDelete", { bg = "#3b1d2e" })
		hl(0, "DiffChange", { bg = "#3b351d" })
		hl(0, "DiffText", { bg = "#3b351d" })

		-- LSP
		hl(0, "LspReferenceText", { bg = "#353a44" })
		hl(0, "LspReferenceRead", { bg = "#353a44" })
		hl(0, "LspReferenceWrite", { bg = "#353a44" })
		hl(0, "LspCodeLens", { fg = palette.bright_black })
		hl(0, "LspCodeLensSeparator", { fg = palette.bright_black })

		-- Telescope
		hl(0, "TelescopeNormal", { bg = palette.bg, fg = palette.fg })
		hl(0, "TelescopeBorder", { bg = palette.bg, fg = palette.bright_black })
		hl(0, "TelescopePromptNormal", { bg = palette.bg, fg = palette.fg })
		hl(0, "TelescopePromptBorder", { bg = palette.bg, fg = palette.blue })
		hl(0, "TelescopeSelection", { bg = "#353a44", fg = palette.fg })
		hl(0, "TelescopeMatching", { fg = palette.yellow, bold = true })

		-- Treesitter context
		hl(0, "TreesitterContext", { bg = palette.bg, fg = palette.bright_black })
		hl(0, "TreesitterContextBottom", { underline = true, sp = palette.bright_black })

		-- Terminal colors
		vim.g.terminal_color_0 = palette.black
		vim.g.terminal_color_1 = palette.red
		vim.g.terminal_color_2 = palette.green
		vim.g.terminal_color_3 = palette.yellow
		vim.g.terminal_color_4 = palette.blue
		vim.g.terminal_color_5 = palette.magenta
		vim.g.terminal_color_6 = palette.cyan
		vim.g.terminal_color_7 = palette.white
		vim.g.terminal_color_8 = palette.bright_black
		vim.g.terminal_color_9 = palette.bright_red
		vim.g.terminal_color_10 = palette.bright_green
		vim.g.terminal_color_11 = palette.bright_yellow
		vim.g.terminal_color_12 = palette.bright_blue
		vim.g.terminal_color_13 = palette.bright_magenta
		vim.g.terminal_color_14 = palette.bright_cyan
		vim.g.terminal_color_15 = palette.bright_white
		vim.g.terminal_color_background = palette.bg
		vim.g.terminal_color_foreground = palette.fg
	end,
}