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
            })

            vim.cmd.colorscheme("gruvbox")

        end,
    }
}
