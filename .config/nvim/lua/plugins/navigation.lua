return {
	{
		"nvim-telescope/telescope.nvim",
		dependencies = { "nvim-lua/plenary.nvim" },
		config = function()
			local builtin = require("telescope.builtin")

			-- Files & Search
			vim.keymap.set("n", "<C-p>", builtin.find_files, { desc = "Find files" })
			vim.keymap.set("n", "<leader>ff", builtin.find_files, { desc = "[F]ind [F]iles" })
			vim.keymap.set("n", "<leader>fg", builtin.live_grep, { desc = "[F]ind by [G]rep (live)" })
			vim.keymap.set("n", "<leader>fw", builtin.grep_string, { desc = "[F]ind [W]ord under cursor" })
			vim.keymap.set("n", "<leader>fl", builtin.current_buffer_fuzzy_find, { desc = "[F]ind [L]ines in buffer" })
			vim.keymap.set("n", "<leader>fr", builtin.oldfiles, { desc = "[F]ind [R]ecent files" })

			-- Buffers
			vim.keymap.set("n", "<leader>;", builtin.buffers, { desc = "Find buffers" })
			vim.keymap.set("n", "<leader>fb", builtin.buffers, { desc = "[F]ind [B]uffers" })

			-- Vim/Help/LSP
			vim.keymap.set("n", "<leader>fh", builtin.help_tags, { desc = "[F]ind [H]elp" })
			vim.keymap.set("n", "<leader>fc", builtin.commands, { desc = "[F]ind [C]ommands" })
			vim.keymap.set("n", "<leader>fk", builtin.keymaps, { desc = "[F]ind [K]eymaps" })
			vim.keymap.set("n", "gd", builtin.lsp_definitions, { desc = "[G]o to [D]efinition" })
			vim.keymap.set("n", "gb", "<C-o>", { desc = "Go back" }) -- Keeping your native Vim jump map

			-- Complex Prompt/Grep Map
			vim.keymap.set("n", "<leader>fa", function()
				local dir = vim.fn.input("Search in directory: ", vim.fn.getcwd(), "dir")
				if dir ~= "" then
					builtin.live_grep({ search_dirs = { dir } })
				end
			end, { desc = "[F]ind [A]nywhere (all files)" })

			-- Neovim Config Search
			vim.keymap.set("n", "<leader>sn", function()
				builtin.find_files({ cwd = vim.fn.stdpath("config") })
			end, { desc = "[S]earch [N]eovim Config" })

			-- Git Integration
			vim.keymap.set("n", "<leader>gC", builtin.git_commits, { desc = "[G]it [C]ommits" })
			vim.keymap.set("n", "<leader>gS", builtin.git_status, { desc = "[G]it [S]tatus" })
			vim.keymap.set("n", "<leader>gb", builtin.git_branches, { desc = "[G]it [B]ranches" })
		end,
	},
	{
		"notjedi/nvim-rooter.lua",
		lazy = false,
		config = function()
			require("nvim-rooter").setup({
				trigger_patterns = { ".git" },
			})
		end,
	},
	{
		"ThePrimeagen/harpoon",
		branch = "harpoon2",
		dependencies = { "nvim-lua/plenary.nvim" },

		config = function()
			local harpoon = require("harpoon")

			harpoon:setup()

			vim.keymap.set("n", "<leader>hh", function()
				harpoon.ui:toggle_quick_menu(harpoon:list())
			end)

			vim.keymap.set("n", "<leader>aa", function()
				harpoon:list():add()
			end)

			vim.keymap.set("n", "<leader>h1", function()
				harpoon:list():select(1)
			end)

			vim.keymap.set("n", "<leader>h2", function()
				harpoon:list():select(2)
			end)

			vim.keymap.set("n", "<leader>h3", function()
				harpoon:list():select(3)
			end)

			vim.keymap.set("n", "<leader>h4", function()
				harpoon:list():select(4)
			end)
		end,
	},
}
