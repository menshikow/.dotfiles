return {
	{
		"nvim-tree/nvim-tree.lua",
		dependencies = { "nvim-tree/nvim-web-devicons" },
		config = function()
			require("nvim-tree").setup({
				view = {
					width = 36,
				},
				filters = {
					dotfiles = false,
				},
			})

			vim.keymap.set("n", "<leader>t", "<CMD>NvimTreeToggle<CR>", { desc = "Toggle [T]ree" })
		end,
	},
}
