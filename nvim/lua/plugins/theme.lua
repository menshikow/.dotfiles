-- return {
--     {
--         "nvim-lualine/lualine.nvim",
--         opts = {
--             options = {
--                 theme = "auto",
--             }
--         }
--     },
--     {
--         "ellisonleao/gruvbox.nvim",
--         name = "gruvbox",
--         priority = 1000,
--         config = function()
--             require("gruvbox").setup({
--                 contrast = "", -- "hard", "soft", or empty
--                 transparent_mode = false,
--                 bold = true,
--                 italic = {
--                     strings = false,
--                     comments = false,
--                     operators = false,
--                     folds = false,
--                 },
--                 overrides = {
--                     -- make background pure black
--                     Normal = { bg = "#000000" },
--                     NormalNC = { bg = "#000000" },
--                     NormalFloat = { bg = "#000000" },
--                     FloatBorder = { bg = "#000000" },
--                     SignColumn = { bg = "#000000" },
--                     VertSplit = { bg = "#000000" },
--                     StatusLine = { bg = "#000000" },
--                     CursorLine = { bg = "#0a0a0a" },
--                     CursorLineNr = { bg = "#000000", fg = "#ebdbb2", bold = true },
--                     MatchParen = { bg = "#2f2f2f", bold = true },
--                     -- Git signs
--                     GitSignsAdd = { fg = "#b8bb26" },
--                     GitSignsChange = { fg = "#d79921" },
--                     GitSignsDelete = { fg = "#fb4934" },
--                 },
--             })
--
--             vim.cmd.colorscheme("gruvbox")
--
--             -- hard override in case something else resets highlights
--             vim.api.nvim_set_hl(0, "Normal", { bg = "#000000" })
--             vim.api.nvim_set_hl(0, "NormalNC", { bg = "#000000" })
--         end,
--     }
-- }
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
        "folke/tokyonight.nvim",
        priority = 1000,
        config = function()
            require("tokyonight").setup({
                style = "night",
                styles = {
                    keywords = { italic = false },
                },
                on_colors = function(colors)
                    colors.git = {
                        -- add = "#82c13e",
                        -- change = "#d4902b",
                        -- delete = "#f10e38",
                        add = colors.green,
                        change = colors.yellow,
                        delete = colors.red,
                    }
                end,
                on_highlights = function(highlights, colors)
                    highlights.MatchParen = {
                        bg = colors.blue0,
                        bold = true,
                    }
                end,
            })
            vim.cmd.colorscheme "tokyonight"
        end
    },
    { "EdenEast/nightfox.nvim" },
    { "vague2k/vague.nvim" },
}
