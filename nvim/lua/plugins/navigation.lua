return {
	{
		"nvim-telescope/telescope.nvim",
		branch = "master",
		dependencies = {
			"nvim-lua/plenary.nvim",
			{
				"nvim-telescope/telescope-fzf-native.nvim",
				build = "make",
				cond = function()
					return vim.fn.executable("make") == 1
				end,
			},

			{
				"nvim-tree/nvim-web-devicons",
			},
		},

		config = function()
			local telescope = require("telescope")
			local builtin = require("telescope.builtin")

			telescope.setup({
				defaults = {

					layout_config = {
						horizontal = {
							preview_width = 0.5,
							width = 0.85,
							height = 0.85,
						},
					},
					path_display = { "smart" },
					selection_caret = "❯ ",
					borderchars = {
						prompt = { "─", "│", "─", "│", "╭", "╮", "╰", "╯" },
						results = { "─", "│", "─", "│", "╭", "╮", "╰", "╯" },
						preview = { "─", "│", "─", "│", "╭", "╮", "╰", "╯" },
					},
				},
			})

			local grep_layout = { layout_strategy = "horizontal", layout_config = { preview_width = 0.5 } }
			local quick_layout = { layout_strategy = "horizontal", previewer = false, layout_config = { width = 0.5 } }

			vim.keymap.set("n", "<leader>ff", function()
				local opts = vim.tbl_deep_extend("force", quick_layout or {}, {
					hidden = false,
					no_ignore = false,
				})

				require("telescope.builtin").find_files(opts)
			end, { desc = "[F]ind [F]iles" })

			vim.keymap.set("n", "<leader>fg", function()
				builtin.live_grep(grep_layout)
			end, { desc = "[F]ind by [G]rep (live)" })

			vim.keymap.set("n", "<leader>fw", function()
				builtin.grep_string(grep_layout)
			end, { desc = "[F]ind [W]ord under cursor" })
			vim.keymap.set("n", "<leader>fb", function()
				builtin.buffers(quick_layout)
			end, { desc = "[F]ind [B]uffers" })
			vim.keymap.set("n", "<leader>fl", function()
				builtin.current_buffer_fuzzy_find(quick_layout)
			end, { desc = "[F]ind [L]ines in buffer" })
			vim.keymap.set("n", "<leader>fh", function()
				builtin.help_tags(quick_layout)
			end, { desc = "[F]ind [H]elp" })
			vim.keymap.set("n", "<leader>fr", function()
				builtin.oldfiles(quick_layout)
			end, { desc = "[F]ind [R]ecent files" })
			vim.keymap.set("n", "<leader>fc", function()
				builtin.commands(quick_layout)
			end, { desc = "[F]ind [C]ommands" })
			vim.keymap.set("n", "<leader>fk", function()
				builtin.keymaps(quick_layout)
			end, { desc = "[F]ind [K]eymaps" })

			vim.keymap.set("n", "gd", function()
				builtin.lsp_definitions(grep_layout)
			end, { desc = "[G]o to [D]efinition" })
			vim.keymap.set("n", "gb", "<C-o>", { desc = "Go back" })

			vim.keymap.set("n", "<leader>fa", function()
				local dir = vim.fn.input("Search in directory: ", vim.fn.getcwd(), "dir")
				if dir ~= "" then
					builtin.live_grep(vim.tbl_deep_extend("force", grep_layout, {
						search_dirs = { dir },
						additional_args = { "--no-ignore", "--hidden" },
					}))
				end
			end, { desc = "[F]ind [A]nywhere (all files)" })
			vim.keymap.set("n", "<leader>sn", function()
				builtin.find_files(vim.tbl_deep_extend("force", quick_layout, { cwd = vim.fn.stdpath("config") }))
			end, { desc = "[S]earch [N]eovim Config" })
			vim.keymap.set("n", "<leader>gC", function()
				builtin.git_commits(quick_layout)
			end, { desc = "[G]it [C]ommits" })
			vim.keymap.set("n", "<leader>gS", function()
				builtin.git_status(quick_layout)
			end, { desc = "[G]it [S]tatus (telescope)" })
			vim.keymap.set("n", "<leader>gb", function()
				builtin.git_branches(quick_layout)
			end, { desc = "[G]it [B]ranches" })

			pcall(telescope.load_extension, "fzf")
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
			end, { desc = "[H]arpoon [H]arpoon" })
			vim.keymap.set("n", "<leader>aa", function()
				harpoon:list():add()
			end, { desc = "[H]arpoon [A]dd" })
			vim.keymap.set("n", "<leader>h1", function()
				harpoon:list():select(1)
			end, { desc = "[H]arpoon [1]" })
			vim.keymap.set("n", "<leader>h2", function()
				harpoon:list():select(2)
			end, { desc = "[H]arpoon [2]" })
			vim.keymap.set("n", "<leader>h3", function()
				harpoon:list():select(3)
			end, { desc = "[H]arpoon [3]" })
			vim.keymap.set("n", "<leader>h4", function()
				harpoon:list():select(4)
			end, { desc = "[H]arpoon [4]" })
		end,
	},
}
