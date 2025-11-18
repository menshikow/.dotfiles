return {
    {
        "blazkowolf/gruber-darker.nvim",
        lazy = false,
        priority = 1000,
        config = function()
            require("gruber-darker").setup({
                italic = {
                    strings = false,
                    comments = false,
                    operators = false,
                    folds = false,
                },
                bold = true,
                underline = true,
                undercurl = true,
                invert = {
                    signs = false,
                    tabline = false,
                    visual = false,
                }
            })

            vim.cmd.colorscheme("gruber-darker")
        end,
    },
}
