-- return {
--     {
--         "blazkowolf/gruber-darker.nvim",
--         lazy = false,
--         priority = 1000,
--         config = function()
--             require("gruber-darker").setup({
--                 italic = {
--                     strings = false,
--                     comments = false,
--                     operators = false,
--                     folds = false,
--                 }, bold = true,
--                 underline = true,
--                 undercurl = true,
--                 invert = {
--                     signs = false,
--                     tabline = false,
--                     visual = false,
--                 }
--             })
--
--             vim.cmd.colorscheme("gruber-darker")
--         end,
--     },
-- }
--
-- return {
--     "rose-pine/neovim",
--     name = "rose-pine",
--     lazy = false,
--     priority = 1000,
--     opts = {
--         variant = "main",
--         dark_variant = "main",
--         dim_inactive_windows = false,
--         extend_background_behind_borders = true,
--
--         styles = {
--             bold = false,
--             italic = false,
--             transparency = false,
--         },
--
--         palette = {
--             main = {
--                 base = "#000000",
--             },
--         },
--
--         highlight_groups = {
--             Normal = { bg = "base", fg = "text" },
--             NormalFloat = { bg = "base" },
--         },
--     },
--     config = function(_, opts)
--         require("rose-pine").setup(opts)
--         vim.cmd("colorscheme rose-pine")
--     end,
-- }
--

return {
	"ellisonleao/gruvbox.nvim",
	lazy = false,
	priority = 1000,
	config = function()
		require("gruvbox").setup({})
		vim.cmd("colorscheme gruvbox")
	end,
}
