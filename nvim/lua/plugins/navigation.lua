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
			local builtin = require("telescope.builtin")
			vim.keymap.set("n", "<leader>ff", builtin.find_files, {})
			vim.keymap.set("n", "<leader>fg", builtin.live_grep, {})
			vim.keymap.set("n", "<leader>fb", builtin.buffers, {})
			vim.keymap.set("n", "<leader>fh", builtin.help_tags, {})
			vim.keymap.set("n", "<leader>fr", builtin.oldfiles, {})
			vim.keymap.set("n", "<leader>fc", builtin.commands, {})
			vim.keymap.set("n", "<leader>fk", builtin.keymaps, {})

			vim.keymap.set("n", "gd", builtin.lsp_definitions, {})
			vim.keymap.set("n", "gb", "<C-o>", { desc = "Go back" })

			-- idk how this is
			vim.keymap.set("n", "<leader>sn", function()
				builtin.find_files({ cwd = vim.fn.stdpath("config") })
			end, { desc = "[S]earch [N]eovim Config" })
			pcall(require("telescope").load_extension, "fzf")
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
			vim.keymap.set("n", "<leader>ha", function()
				harpoon:list():add()
			end, { desc = "[H]arpoon [A]dd" })
			vim.keymap.set("n", "<leader>ha", function()
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

			-- TODO: harpoon + telescope integration

			-- local harpoon = require('harpoon')
			-- harpoon:setup({})
			--
			-- -- basic telescope configuration
			-- local conf = require("telescope.config").values
			-- local function toggle_telescope(harpoon_files)
			--     local file_paths = {}
			--     for _, item in ipairs(harpoon_files.items) do
			--         table.insert(file_paths, item.value)
			--     end
			--
			--     require("telescope.pickers").new({}, {
			--         prompt_title = "Harpoon",
			--         finder = require("telescope.finders").new_table({
			--             results = file_paths,
			--         }),
			--         previewer = conf.file_previewer({}),
			--         sorter = conf.generic_sorter({}),
			--     }):find()
			-- end
			--
			-- vim.keymap.set("n", "<C-e>", function() toggle_telescope(harpoon:list()) end,
			--     { desc = "Open harpoon window" })
		end,
	},
}
