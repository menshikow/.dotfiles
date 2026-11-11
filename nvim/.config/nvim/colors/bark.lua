-- Bark ships its Vim colorscheme at the repository root instead of colors/.
vim.cmd.runtime("bark.vim")
vim.g.colors_name = "bark"

-- Muted completion windows without Bark's reversed popup selection.
local completion_highlights = {
        BlinkCmpMenu = { fg = "#b0b0b0", bg = "#181818" },
        BlinkCmpMenuBorder = { fg = "#2b2b2b", bg = "#181818" },
        BlinkCmpMenuSelection = { fg = "#d0d0d0", bg = "#2b2b2b" },
        BlinkCmpLabel = { fg = "#b0b0b0" },
        BlinkCmpLabelMatch = { fg = "#d0d0d0" },
        BlinkCmpLabelDetail = { fg = "#808080" },
        BlinkCmpLabelDescription = { link = "BlinkCmpLabelDetail" },
        BlinkCmpLabelDeprecated = { fg = "#808080", strikethrough = true },
        BlinkCmpSource = { link = "BlinkCmpLabelDetail" },
        BlinkCmpKind = { link = "BlinkCmpLabelDetail" },
        BlinkCmpScrollBarThumb = { bg = "#505050" },
        BlinkCmpScrollBarGutter = { bg = "#181818" },
        BlinkCmpDoc = { link = "BlinkCmpMenu" },
        BlinkCmpDocBorder = { link = "BlinkCmpMenuBorder" },
        BlinkCmpDocSeparator = { link = "BlinkCmpMenuBorder" },
        BlinkCmpDocCursorLine = { bg = "#202020" },
}
for name, highlight in pairs(completion_highlights) do
        vim.api.nvim_set_hl(0, name, highlight)
end

-- Keep Bark's colors and other styles, but never render italic text.
for name, highlight in pairs(vim.api.nvim_get_hl(0, { link = true })) do
        if highlight.italic or (highlight.cterm and highlight.cterm.italic) then
                highlight.italic = false
                if highlight.cterm then
                        highlight.cterm.italic = false
                end
                vim.api.nvim_set_hl(0, name, highlight)
        end
end
