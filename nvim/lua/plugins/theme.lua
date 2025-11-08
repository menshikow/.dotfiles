return {
    {
        "rose-pine/neovim",
        name = "rose-pine",
        priority = 1000,
        config = function()
            require("rose-pine").setup({
                variant = "main", -- or "moon" / "dawn"
                dark_variant = "main",
                bold_vert_split = false,
                dim_nc_background = false,
                disable_background = false,
                disable_float_background = false,
                disable_italics = true,
                highlight_groups = {
                    Normal = { bg = "#000000" },
                    NormalNC = { bg = "#000000" },
                    NormalFloat = { bg = "#000000" },
                    FloatBorder = { bg = "#000000" },
                    SignColumn = { bg = "#000000" },
                    VertSplit = { bg = "#000000" },
                    StatusLine = { bg = "#000000" },
                    CursorLine = { bg = "#0a0a0a" },
                    CursorLineNr = { bg = "#000000", fg = "#e0def4", bold = true },
                    MatchParen = { bg = "#2f2f2f", bold = true },
                    GitSignsAdd = { fg = "#9ccfd8" },
                    GitSignsChange = { fg = "#f6c177" },
                    GitSignsDelete = { fg = "#eb6f92" },
                },
            })

            vim.cmd("colorscheme rose-pine")
        end,
    },
}

