return {
    {
        "nvim-lualine/lualine.nvim",
        opts = {
            options = {
                theme = "auto",
            }
        }
    },
    {
        "ellisonleao/gruvbox.nvim",
        name = "gruvbox",
        priority = 1000,
        config = function()
            require("gruvbox").setup({
                contrast = "", -- "hard", "soft", or empty
                transparent_mode = false,
                bold = true,
                italic = {
                    strings = false,
                    comments = false,
                    operators = false,
                    folds = false,
                },
                -- overrides = {
                --     -- make background pure black
                --     Normal = { bg = "#000000" },
                --     NormalNC = { bg = "#000000" },
                --     NormalFloat = { bg = "#000000" },
                --     FloatBorder = { bg = "#000000" },
                --     SignColumn = { bg = "#000000" },
                --     VertSplit = { bg = "#000000" },
                --     StatusLine = { bg = "#000000" },
                --     CursorLine = { bg = "#0a0a0a" },
                --     CursorLineNr = { bg = "#000000", fg = "#ebdbb2", bold = true },
                --     MatchParen = { bg = "#2f2f2f", bold = true },
                --     -- Git signs
                --     GitSignsAdd = { fg = "#b8bb26" },
                --     GitSignsChange = { fg = "#d79921" },
                --     GitSignsDelete = { fg = "#fb4934" },
                -- },
            })

            vim.cmd.colorscheme("gruvbox")

        end,
    }
}
