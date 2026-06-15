return {
	{
		"ibhagwan/fzf-lua",
		dependencies = { "nvim-tree/nvim-web-devicons" },
		config = function()
			local fzf = require("fzf-lua")

			-- Base setup (Stop putting a giant window over my editor)
			fzf.setup({
				winopts = {
					split = "belowright 10new",
					preview = {
						hidden = true,
					},
				},
				files = {
					-- file icons are distracting
					file_icons = false,
					-- git icons are nice
					git_icons = true,
					-- but don't mess up my anchored search
					_fzf_nth_devicons = true,
				},
				buffers = {
					file_icons = false,
					git_icons = true,
					-- no nth_devicons as we'll do that manually since we also use with-nth
				},
				fzf_opts = {
					-- no reverse view
					["--layout"] = "default",
				},
			})

			-- when using C-p for quick file open, pass the file list through
			-- https://github.com/jonhoo/proximity-sort
			-- to prefer files closer to the current file.
			vim.keymap.set("", "<C-p>", function()
				local opts = {}
				opts.cmd = "fd --color=never --hidden --type f --type l --exclude .git"
				local base = vim.fn.fnamemodify(vim.fn.expand("%"), ":h:.:S")
				if base ~= "." then
					local prox_sort_path = "~/.dotfiles/scripts/proximity-sort"
					opts.cmd = opts.cmd .. (" | %s %s"):format(prox_sort_path, vim.fn.shellescape(vim.fn.expand("%")))
				end
				opts.fzf_opts = {
					["--scheme"] = "path",
					["--tiebreak"] = "index",
					["--layout"] = "default",
				}
				fzf.files(opts)
			end, { desc = "Find files (proximity-sort)" })

			-- Custom buffer search logic
			local function custom_buffer_search()
				fzf.buffers({
					-- just include the paths in the fzf bits, and nothing else
					-- https://github.com/ibhagwan/fzf-lua/issues/2230#issuecomment-3164258823
					fzf_opts = {
						["--with-nth"] = "{-3..-2}",
						["--nth"] = "-1",
						["--delimiter"] = "[:\u{2002}]",
						["--header-lines"] = "false",
					},
					header = false,
				})
			end

			-- fzf for buffers
			vim.keymap.set("n", "<leader>;", custom_buffer_search, { desc = "Find buffers (custom)" })
			vim.keymap.set("n", "<leader>fb", custom_buffer_search, { desc = "[F]ind [B]uffers" })

			-- ported keymaps form telescope

			-- Files & Search
			vim.keymap.set("n", "<leader>ff", function()
				fzf.files({ hidden = false })
			end, { desc = "[F]ind [F]iles" })
			vim.keymap.set("n", "<leader>fg", fzf.live_grep, { desc = "[F]ind by [G]rep (live)" })
			vim.keymap.set("n", "<leader>fw", fzf.grep_cword, { desc = "[F]ind [W]ord under cursor" })
			vim.keymap.set("n", "<leader>fl", fzf.blines, { desc = "[F]ind [L]ines in buffer" })
			vim.keymap.set("n", "<leader>fr", fzf.oldfiles, { desc = "[F]ind [R]ecent files" })

			-- Vim/Help/LSP
			vim.keymap.set("n", "<leader>fh", fzf.help_tags, { desc = "[F]ind [H]elp" })
			vim.keymap.set("n", "<leader>fc", fzf.commands, { desc = "[F]ind [C]ommands" })
			vim.keymap.set("n", "<leader>fk", fzf.keymaps, { desc = "[F]ind [K]eymaps" })
			vim.keymap.set("n", "gd", fzf.lsp_definitions, { desc = "[G]o to [D]efinition" })
			vim.keymap.set("n", "gb", "<C-o>", { desc = "Go back" }) -- Keeping your native Vim jump map

			-- Complex Prompt/Grep Map
			vim.keymap.set("n", "<leader>fa", function()
				local dir = vim.fn.input("Search in directory: ", vim.fn.getcwd(), "dir")
				if dir ~= "" then
					fzf.live_grep({
						cwd = dir,
						rg_opts = "--no-ignore --hidden --column --line-number --no-heading --color=always --smart-case",
					})
				end
			end, { desc = "[F]ind [A]nywhere (all files)" })

			-- Neovim Config Search
			vim.keymap.set("n", "<leader>sn", function()
				fzf.files({ cwd = vim.fn.stdpath("config") })
			end, { desc = "[S]earch [N]eovim Config" })

			-- Git Integration
			vim.keymap.set("n", "<leader>gC", fzf.git_commits, { desc = "[G]it [C]ommits" })
			vim.keymap.set("n", "<leader>gS", fzf.git_status, { desc = "[G]it [S]tatus" })
			vim.keymap.set("n", "<leader>gb", fzf.git_branches, { desc = "[G]it [B]ranches" })
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
