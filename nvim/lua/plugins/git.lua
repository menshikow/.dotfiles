return {
	{
		"NeogitOrg/neogit",
		dependencies = {
			"nvim-lua/plenary.nvim",
			"sindrets/diffview.nvim",
			"nvim-telescope/telescope.nvim",
		},
		cmd = "Neogit",
		config = function()
			local neogit = require("neogit")

			neogit.setup({
				graph_style = "unicode",
				integrations = { diffview = true },
				kind = "tab",
				disable_builtin_notifications = true,
				status = { recent_commit_count = 20 },
				sections = {
					recent = { folded = false },
					untracked = { folded = false },
					staged = { folded = false },
					unstaged = { folded = false },
				},
			})

			vim.keymap.set("n", "<leader>gs", "<CMD>Neogit kind=tab<CR>", { desc = "[G]it [S]tatus" })
			vim.keymap.set("n", "<leader>gl", "<CMD>Neogit kind=tab log<CR>", { desc = "[G]it [L]og" })
			vim.keymap.set("n", "<leader>gd", "<CMD>DiffviewOpen<CR>", { desc = "[G]it [D]iffview" })
		end,
	},
}
