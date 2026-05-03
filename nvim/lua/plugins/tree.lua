return {
	"nvim-tree/nvim-tree.lua",
	version = "*",
	lazy = false,
	dependencies = {
		"nvim-tree/nvim-web-devicons",
	},

	keys = {
		{ "<leader>e", ":NvimTreeToggle<CR>", desc = "Toggle NvimTree" },

		{
			"<leader>t",
			function()
				require("nvim-tree.api").tree.focus()
			end,
			desc = "Focus NvimTree",
		},
	},

	config = function()
		require("nvim-tree").setup({
			disable_netrw = false,
			hijack_netrw = false,
		})
	end,
}
