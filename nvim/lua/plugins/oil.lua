return {
	{
		"stevearc/oil.nvim",
		dependencies = { "nvim-tree/nvim-web-devicons" },
		config = function()
			require("oil").setup({
				default_file_explorer = true,
				delete_to_trash = true,
				skip_confirm_for_simple_edits = true,
				view_options = {
					show_hidden = true,
					natural_order = true,
				},
			})

			vim.keymap.set("n", "<leader>e", "<CMD>Oil<CR>", { desc = "Open file [E]xplorer" })
			vim.keymap.set("n", "<leader>o", "<CMD>Oil<CR>", { desc = "Open [O]il" })
			vim.keymap.set("n", "-", "<CMD>Oil<CR>", { desc = "Open parent directory in Oil" })
		end,
	},
}
