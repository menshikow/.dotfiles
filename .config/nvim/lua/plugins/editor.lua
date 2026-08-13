return {
	{
		"lewis6991/gitsigns.nvim",
		opts = {},
	},
	{
		"windwp/nvim-autopairs",
		event = "InsertEnter",
		config = true,
	},
	{
		"folke/todo-comments.nvim",
		dependencies = { "nvim-lua/plenary.nvim" },
		opts = {},
	},
	{
		"mg979/vim-visual-multi",
		branch = "master",
		init = function()
			pcall(vim.keymap.del, "n", "<C-n>")
			pcall(vim.keymap.del, "x", "<C-n>")

			vim.g.VM_maps = {
				["Find Under"] = "<C-n>",
				["Find Subword Under"] = "<C-n>",
			}
		end,
	},
}
