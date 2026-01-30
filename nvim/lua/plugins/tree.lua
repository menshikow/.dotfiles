return {
	{
		"nvim-tree/nvim-tree.lua",
		dependencies = {
			"nvim-tree/nvim-web-devicons",
		},
		config = function()
			require("nvim-tree").setup({
				sort = {
					sorter = "case_sensitive",
				},
				view = {
					width = 30,
					side = "left",
				},
				renderer = {
					group_empty = true,
					icons = {
						show = {
							file = true,
							folder = true,
							folder_arrow = true,
							git = true,
						},
					},
				},
				filters = {
					dotfiles = false,
					custom = { ".git", "node_modules", ".cache" },
				},
				git = {
					enable = true,
					ignore = false,
				},
				actions = {
					open_file = {
						quit_on_open = false,
						resize_window = true,
					},
				},
			})
			
			vim.keymap.set("n", "<leader>j", "<cmd>NvimTreeToggle<CR>", { noremap = true, silent = true, desc = "Toggle NvimTree" })
		end,
	},
}