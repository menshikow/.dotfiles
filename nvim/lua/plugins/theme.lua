return {
    {
        "folke/tokyonight.nvim",
        name = "tokyonight",
        priority = 1000,
        config = function()
            require("tokyonight").setup({
                style = "storm", -- "storm", "moon", "day"
                transparent = false,
                terminal_colors = true,
                styles = {
                    comments = { italic = false },
                    keywords = { italic = false },
                    functions = { bold = false },
                    variables = {},
                },
                on_highlights = function(hl, c)
                    hl.Normal = { bg = "#000000", fg = c.fg }
                    hl.NormalNC = { bg = "#000000", fg = c.fg }
                    hl.NormalFloat = { bg = "#000000", fg = c.fg }
                    hl.FloatBorder = { bg = "#000000", fg = c.blue }
                    hl.SignColumn = { bg = "#000000" }
                    hl.VertSplit = { bg = "#000000", fg = c.blue }
                    hl.StatusLine = { bg = "#000000", fg = c.fg }
                    hl.CursorLine = { bg = "#0a0a0a" }
                    hl.CursorLineNr = { bg = "#000000", fg = c.fg, bold = true }
                    hl.MatchParen = { bg = "#2f2f2f", bold = true }
                    hl.GitSignsAdd = { fg = c.green }
                    hl.GitSignsChange = { fg = c.yellow }
                    hl.GitSignsDelete = { fg = c.red }
                end,
            })

            vim.cmd("colorscheme tokyonight")
        end,
    },
}

