-- Recolor Bark's highlights, including its completion overrides.
vim.cmd.runtime("colors/bark.lua")

-- https://www.nordtheme.com/docs/colors-and-palettes/
local palette = {
        [0x101010] = 0x000000,
        [0x181818] = 0x353c4a,
        [0x202020] = 0x3b4252,
        [0x2b2b2b] = 0x434c5e,
        [0x505050] = 0x4c566a,
        [0x808080] = 0x9aa9bf,
        [0xb0b0b0] = 0xd8dee9,
        [0xd0d0d0] = 0xe5e9f0,
        [0xe0e0e0] = 0xeceff4,
        [0xfafafa] = 0xeceff4,
        [0x7cafc2] = 0x88c0d0,
        [0x86c1b9] = 0x8fbcbb,
        [0xa16946] = 0xd08770,
        [0xa1b56c] = 0xa3be8c,
        [0xab4642] = 0xbf616a,
        [0xba8baf] = 0x81a1c1,
        [0xdc9656] = 0xb48ead,
        [0xf7ca88] = 0xebcb8b,
}

-- Keep floating menus and the gutter neutral, with Nord accents in the buffer.
local overrides = {
        NormalFloat = { bg = 0x000000 },
        Visual = { bg = 0x303030 },
        Pmenu = { bg = 0x181818 },
        PmenuMatch = { bg = 0x181818 },
        PmenuKind = { bg = 0x181818 },
        PmenuExtra = { bg = 0x181818 },
        PmenuSel = { bg = 0x303030, reverse = false },
        PmenuMatchSel = { bg = 0x303030, reverse = false },
        PmenuKindSel = { bg = 0x303030, reverse = false },
        PmenuExtraSel = { bg = 0x303030, reverse = false },
        PmenuSbar = { bg = 0x181818 },
        BlinkCmpMenu = { bg = 0x181818 },
        BlinkCmpMenuBorder = { fg = 0x505050, bg = 0x181818 },
        BlinkCmpMenuSelection = { bg = 0x303030 },
        BlinkCmpScrollBarGutter = { bg = 0x181818 },
        BlinkCmpScrollBarThumb = { bg = 0x505050 },
        BlinkCmpDocCursorLine = { bg = 0x303030 },
        CursorLineNr = { bg = 0x000000 },
        CursorLineSign = { bg = 0x000000 },
        CursorLineFold = { bg = 0x000000 },
        GitSignsAdd = { fg = 0x8a8a8a, bg = 0x000000 },
        GitSignsChange = { fg = 0x8a8a8a, bg = 0x000000 },
        GitSignsDelete = { fg = 0x8a8a8a, bg = 0x000000 },
        GitSignsUntracked = { fg = 0x8a8a8a, bg = 0x000000 },
        Comment = { fg = 0x8190a6 },
        Type = { fg = 0x8fbcbb },
        Typedef = { fg = 0x8fbcbb },
        Statement = { fg = 0x81a1c1 },
        Repeat = { fg = 0x81a1c1 },
        Operator = { fg = 0x81a1c1 },
        IncSearch = { bg = 0x88c0d0 },
        CurSearch = { bg = 0x88c0d0 },
        DiagnosticWarn = { fg = 0xebcb8b },
        DiagnosticFloatingWarn = { fg = 0xebcb8b },
        DiagnosticUnderlineWarn = { sp = 0xebcb8b },
}

for name, highlight in pairs(vim.api.nvim_get_hl(0, { link = true })) do
        if not highlight.link then
                local override = overrides[name]
                local changed = false
                for _, attribute in ipairs({ "fg", "bg", "sp" }) do
                        local replacement = palette[highlight[attribute]]
                        if replacement then
                                highlight[attribute] = replacement
                                changed = true
                        end
                end
                if override then
                        for attribute, value in pairs(override) do
                                highlight[attribute] = value
                        end
                        changed = true
                end
                if changed then
                        vim.api.nvim_set_hl(0, name, highlight)
                end
        end
end

-- Match the divider to the black background on both sides of Neo-tree.
vim.api.nvim_set_hl(0, "NeoTreeWinSeparator", { fg = 0x000000, bg = 0x000000 })

vim.g.colors_name = "bark-nord"
