return {
	"folke/which-key.nvim",
	event = "VeryLazy",
	keys = {
		"<leader>",
		"<localleader>",
		"g",
		"]",
		"[",
	},
	opts = {
		preset = "helix",
		spec = {
			{ "<leader>f", group = "find" },
			{ "<leader>g", group = "git" },
			{ "<leader>s", group = "window split" },
			{ "<leader>h", group = "harpoon" },
			{ "<leader>d", group = "debug" },
			{ "[", group = "prev" },
			{ "]", group = "next" },
		},
	},
}
