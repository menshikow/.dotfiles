local transparent = vim.g.dark_transp_bg == 1

local c = {
	black = "#101010",
	darkgrey = "#252525",
	darkstone = "#7c7c7c",
	almostwhite = "#b9b9b9",
	grey = "#8e8e8e",
	white = "#f7f7f7",
	beige = "#e3e3e3",
	red = "#ce5252",
	green = "#8c9440",
	blue = "#5f819d",
	yellow = "#f0c674",
	none = "NONE",
}

local bg = transparent and c.none or c.black

vim.o.background = "dark"
vim.g.colors_name = "austere_nvim"
vim.cmd("highlight clear")
if vim.fn.exists("syntax_on") == 1 then
	vim.cmd("syntax reset")
end

local function set(group, spec)
	vim.api.nvim_set_hl(0, group, spec)
end

local function link(group, target)
	vim.api.nvim_set_hl(0, group, { link = target })
end

set("Normal", { fg = c.almostwhite, bg = bg })
set("Terminal", { fg = c.almostwhite, bg = bg })
set("ColorColumn", { bg = c.black })
set("Conceal", {})
set("Cursor", { fg = c.black, bg = c.white })
set("CursorColumn", { bg = c.black })
set("CursorLine", { bg = c.black })
set("CursorLineNr", { fg = c.almostwhite, bg = c.black })
set("DiffAdd", { fg = c.green, bg = c.black, reverse = true })
set("DiffChange", { fg = c.yellow, bg = c.black, reverse = true })
set("DiffDelete", { fg = c.red, bg = c.black, reverse = true })
set("DiffText", { fg = c.almostwhite, bg = c.black, bold = true, reverse = true })
set("Directory", { fg = c.almostwhite, bg = c.black })
set("EndOfBuffer", { fg = c.almostwhite, bg = c.black })
set("ErrorMsg", { fg = c.red, bg = c.black, reverse = true })
set("FoldColumn", { fg = c.almostwhite, bg = c.darkgrey })
set("Folded", { fg = c.almostwhite, bg = c.darkgrey, italic = true })
set("IncSearch", { fg = c.green, bg = c.black, bold = true })
set("LineNr", { fg = c.almostwhite, bg = c.black })
set("MatchParen", { fg = c.almostwhite, bg = c.black })
set("ModeMsg", { fg = c.almostwhite, bg = c.black })
set("MoreMsg", { fg = c.almostwhite, bg = c.black })
set("NonText", { fg = c.beige })
set("Pmenu", { fg = c.almostwhite, bg = c.darkgrey })
set("PmenuSbar", { fg = c.almostwhite, bg = c.darkgrey })
set("PmenuSel", { fg = c.white, bg = c.darkgrey })
set("PmenuThumb", { fg = c.almostwhite, bg = c.black })
set("Question", { fg = c.almostwhite, bg = c.black })
set("Search", { fg = c.green, bg = c.black })
set("SignColumn", { fg = c.almostwhite, bg = c.black })
set("SpecialKey", { fg = c.beige, bg = c.black })
set("SpellBad", { fg = c.red, bg = c.black, sp = c.red, undercurl = true })
set("SpellCap", { fg = c.red, bg = c.black, sp = c.blue, undercurl = true })
set("SpellLocal", { fg = c.red, bg = c.black, sp = c.darkstone, undercurl = true })
set("SpellRare", { fg = c.beige, bg = c.black, sp = c.beige, reverse = true })
set("StatusLine", { fg = c.almostwhite, bg = c.black })
set("StatusLineNC", { fg = c.almostwhite, bg = c.black })
set("TabLine", { fg = c.almostwhite, bg = c.black })
set("TabLineFill", { fg = c.almostwhite, bg = c.black })
set("TabLineSel", { fg = c.almostwhite, bg = c.black })
set("Title", { fg = c.darkstone, bg = c.black })
set("VertSplit", { fg = c.black, bg = c.black })
set("Visual", { fg = c.almostwhite, bg = c.darkgrey })
set("VisualNOS", { fg = c.almostwhite, bg = c.darkgrey })
set("WarningMsg", { fg = c.almostwhite, bg = c.black })
set("WildMenu", { fg = c.almostwhite, bg = c.black })

set("Comment", { fg = c.darkstone, italic = true })
set("Constant", { fg = c.grey, italic = true })
set("Error", { fg = c.red, bg = c.black, reverse = true })
set("Identifier", { fg = c.almostwhite })
set("Ignore", { fg = c.almostwhite })
set("PreProc", { fg = c.grey })
set("Special", { fg = c.almostwhite })
set("Statement", { fg = c.grey })
set("Todo", { fg = c.almostwhite })
set("Type", { fg = c.almostwhite })
set("Underlined", { fg = c.grey, underline = true })
set("CursorIM", { fg = c.black, bg = c.white })
set("ToolbarLine", { bg = c.black })
set("ToolbarButton", { fg = c.almostwhite, bg = c.black, bold = true })

set("diffAdded", { fg = c.green, bg = c.black })
set("diffRemoved", { fg = c.red, bg = c.black })
set("CtrlPPrtText", { fg = c.green, bg = c.black })
set("CtrlPMatch", { fg = c.green, bg = c.black })
set("CtrlPPrtBase", { fg = c.green, bg = c.black })
set("CtrlPLinePre", { fg = c.yellow, bg = c.black })
set("CtrlPPrtCursor", { fg = c.yellow, bg = c.black })
set("mkdItalic", { fg = c.grey, bg = c.black })
set("mkdCode", { fg = c.grey, bg = c.black })
set("mkdSnippetSH", { fg = c.grey, bg = c.black })
set("NERDTreeFile", { fg = c.almostwhite, bg = c.black })
set("NERDTreeExecFile", { fg = c.almostwhite, bg = c.black })

set("pythonStatement", { fg = c.white, bg = c.black, bold = true })
set("pythonConditional", { fg = c.white, bg = c.black, bold = true })
set("pythonRepeat", { fg = c.white, bg = c.black, bold = true })
set("pythonOperator", { fg = c.white, bg = c.black, bold = true })
set("pythonException", { fg = c.white, bg = c.black, bold = true })
set("pythonInclude", { fg = c.white, bg = c.black, bold = true })
set("pythonAsync", { fg = c.white, bg = c.black, bold = true })
set("pythonEscape", { fg = c.white, bg = c.black, bold = true })
set("pythonBuiltin", { fg = c.white, bg = c.black, bold = true })
set("pythonFunction", { fg = c.white, bg = c.black, bold = true })

set("ALEVirtualTextError", { fg = c.red, bg = c.darkgrey })
set("ALEVirtualTextWarning", { fg = c.yellow, bg = c.darkgrey })
set("ALEVirtualTextInfo", { fg = c.blue, bg = c.darkgrey })
set("ALEVirtualTextStyleError", { fg = c.red, bg = c.darkgrey })
set("ALEVirtualTextStyleWarning", { fg = c.yellow, bg = c.darkgrey })
set("HighlightedyankRegion", { fg = c.white, bg = c.beige })

set("@comment", { link = "Comment" })
set("@keyword", { link = "Statement" })
set("@keyword.function", { link = "Statement" })
set("@keyword.return", { link = "Statement" })
set("@function", { link = "Identifier" })
set("@function.builtin", { link = "Identifier" })
set("@string", { link = "Constant" })
set("@number", { link = "Constant" })
set("@type", { link = "Type" })
set("@type.builtin", { link = "Type" })
set("@variable", { link = "Identifier" })

set("DiagnosticError", { fg = c.red })
set("DiagnosticWarn", { fg = c.yellow })
set("DiagnosticInfo", { fg = c.blue })
set("DiagnosticHint", { fg = c.beige })

set("DiagnosticUnderlineError", { sp = c.red, undercurl = true })
set("DiagnosticUnderlineWarn", { sp = c.yellow, undercurl = true })
set("DiagnosticUnderlineInfo", { sp = c.blue, undercurl = true })
set("DiagnosticUnderlineHint", { sp = c.beige, undercurl = true })

link("QuickFixLine", "Search")
link("StatusLineTerm", "StatusLine")
link("StatusLineTermNC", "StatusLineNC")
link("Boolean", "Constant")
link("Character", "Constant")
link("Conditional", "Statement")
link("Define", "PreProc")
link("Debug", "Special")
link("Delimiter", "Special")
link("Exception", "Statement")
link("Float", "Constant")
link("Function", "Identifier")
link("Include", "PreProc")
link("Keyword", "Statement")
link("Label", "Statement")
link("Macro", "PreProc")
link("Number", "Constant")
link("Operator", "Statement")
link("PreCondit", "PreProc")
link("Repeat", "Statement")
link("SpecialChar", "Special")
link("SpecialComment", "Special")
link("StorageClass", "Type")
link("String", "Constant")
link("Structure", "Type")
link("Tag", "Special")
link("Typedef", "Type")
link("lCursor", "Cursor")
