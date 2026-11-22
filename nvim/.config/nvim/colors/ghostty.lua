vim.o.background = "dark"
vim.cmd.highlight("clear")
vim.g.colors_name = "ghostty"

-- The 16 ANSI slots from the Ghostty config, in terminal order.
local ansi = {
        [0] = "#242424", "#b85c5c", "#76ad57", "#b6a360", "#6588ad", "#9873a2", "#579692", "#b5b5b5",
        "#666666", "#d87979", "#94c875", "#d5c27e", "#83a4cb", "#b18fbc", "#76b9b1", "#dedede",
}
for index = 0, 15 do
        vim.g["terminal_color_" .. index] = ansi[index]
end

local bg, fg = "#000000", ansi[7]
local surface, selection, bright = ansi[0], "#3a3a3a", ansi[15]
local highlights = {
        Normal = { fg = fg, bg = bg },
        Cursor = { fg = bg, bg = fg },
        Visual = { fg = bright, bg = selection },
        WinSeparator = { fg = selection, bg = bg },
        NeoTreeWinSeparator = { fg = bg, bg = bg },
        LineNr = { fg = ansi[8] },
        CursorLineNr = { fg = bright, bold = true },
        CursorLine = { bg = surface },
        CursorColumn = { link = "CursorLine" },
        ColorColumn = { bg = surface },
        SignColumn = { bg = bg },
        NonText = { fg = ansi[8] },
        Conceal = { link = "NonText" },
        Folded = { fg = fg, bg = surface },
        NormalFloat = { fg = fg, bg = surface },
        FloatBorder = { fg = ansi[8], bg = surface },
        Pmenu = { link = "NormalFloat" },
        PmenuSel = { link = "Visual" },
        PmenuThumb = { bg = ansi[8] },
        PmenuSbar = { bg = surface },
        BlinkCmpMenuBorder = { link = "FloatBorder" },
        BlinkCmpMenuSelection = { link = "Visual" },
        StatusLine = { fg = bright, bg = selection },
        StatusLineNC = { fg = fg, bg = surface },
        WinBar = { link = "StatusLine" },
        WinBarNC = { link = "StatusLineNC" },
        QuickFixLine = { link = "Visual" },
        Search = { fg = bg, bg = ansi[3] },
        CurSearch = { fg = bg, bg = ansi[11], bold = true },
        MatchParen = { fg = bright, bg = selection, bold = true },
        MatchWord = { fg = ansi[13] },
        MatchWordCur = { link = "MatchWord" },

        Comment = { fg = ansi[8] },
        Identifier = { fg = fg },
        ["@variable"] = { link = "Identifier" },
        Function = { fg = ansi[12] },
        Statement = { fg = ansi[5] },
        Operator = { fg = fg },
        PreProc = { fg = ansi[3] },
        Type = { fg = ansi[6] },
        Special = { fg = ansi[14] },
        Constant = { fg = ansi[13] },
        String = { fg = ansi[2] },
        Character = { link = "String" },
        Number = { fg = ansi[11] },
        Boolean = { link = "Constant" },
        Delimiter = { fg = fg },
        Directory = { fg = ansi[12] },
        Title = { fg = bright, bold = true },
        Todo = { fg = ansi[11], bold = true },
        Added = { fg = ansi[2] },
        Changed = { fg = ansi[4] },
        Removed = { fg = ansi[1] },
        DiffAdd = { fg = ansi[10], bg = surface },
        DiffChange = { fg = ansi[12], bg = surface },
        DiffDelete = { fg = ansi[9], bg = surface },
        DiffText = { fg = bright, bg = selection, bold = true },
        Error = { fg = ansi[9], bold = true },
        ErrorMsg = { link = "DiagnosticError" },
        WarningMsg = { link = "DiagnosticWarn" },
        OkMsg = { link = "DiagnosticOk" },
        ModeMsg = { link = "DiagnosticOk" },
        MoreMsg = { link = "DiagnosticHint" },
        Question = { link = "DiagnosticHint" },
        SpellBad = { link = "DiagnosticUnderlineError" },
        SpellCap = { link = "DiagnosticUnderlineWarn" },
        SpellRare = { link = "DiagnosticUnderlineInfo" },
        SpellLocal = { link = "DiagnosticUnderlineOk" },
        DiagnosticDeprecated = { strikethrough = true, sp = ansi[9] },
}
for severity, color in pairs({ Error = ansi[9], Warn = ansi[11], Info = ansi[12], Hint = ansi[14], Ok = ansi[10] }) do
        highlights["Diagnostic" .. severity] = { fg = color }
        highlights["DiagnosticUnderline" .. severity] = { undercurl = true, sp = color }
end
for name, highlight in pairs(highlights) do
        vim.api.nvim_set_hl(0, name, highlight)
end
