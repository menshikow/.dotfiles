return {
	{
		"mfussenegger/nvim-dap",
		dependencies = {
			"igorlfs/nvim-dap-view",
			"theHamsta/nvim-dap-virtual-text",
			"nvim-neotest/nvim-nio",
			"williamboman/mason.nvim",
		},
		config = function()
			local dap = require("dap")
			local dapview = require("dap-view")

			require("dap-view").setup()
			require("nvim-dap-virtual-text").setup()

			local last_program = vim.fn.getcwd()
			local read_program = function()
				last_program = vim.fn.input("Run: ", last_program, "file")
				return last_program
			end

			local last_args = ""
			local read_args = function()
				last_args = vim.fn.input("Args: ", last_args, "file")
				return vim.split(last_args, " ")
			end

			dap.adapters.codelldb = {
				type = "executable",
				command = "codelldb",
			}

			dap.configurations.cpp = {
				{
					name = "Launch program",
					type = "codelldb",
					request = "launch",
					program = read_program,
					args = read_args,
					cwd = "${workspaceFolder}",
					stopOnEntry = false,
				},
				{
					name = "Launch last",
					type = "codelldb",
					request = "launch",
					program = function()
						return last_program
					end,
					args = function()
						return vim.split(last_args, " ")
					end,
					cwd = "${workspaceFolder}",
					stopOnEntry = false,
				},
			}
			dap.configurations.c = dap.configurations.cpp
			dap.configurations.rust = dap.configurations.cpp

			dap.adapters.python = {
				type = "executable",
				command = "python3",
				args = { "-m", "debugpy.adapter" },
			}

			dap.configurations.python = {
				{
					type = "python",
					request = "launch",
					name = "Launch current file",
					program = "${file}",
					-- Don't let debugpy hijack stdin/stdout
					console = "integratedTerminal",
				},
				{
					type = "python",
					request = "launch",
					name = "Launch with args",
					program = function()
						return vim.fn.input("Program: ", "${file}", "file")
					end,
					args = function()
						return vim.fn.split(vim.fn.input("Args: "), " ")
					end,
					console = "integratedTerminal",
				},
			}

			dap.adapters.ocaml = {
				type = "executable",
				command = "ocamlearlybird",
			}

			dap.configurations.ocaml = {
				{
					type = "ocaml",
					request = "launch",
					name = "Launch current file",
					program = "${file}",
				},
				{
					type = "ocaml",
					request = "launch",
					name = "Launch with args",
					program = function()
						return vim.fn.input("Program: ", "${file}", "file")
					end,
					args = function()
						return vim.fn.split(vim.fn.input("Args: "), " ")
					end,
				},
			}

			vim.keymap.set("n", "<leader>db", dap.toggle_breakpoint)
			vim.keymap.set("n", "<leader>dd", dap.run_to_cursor)
			vim.keymap.set("n", "<F1>", dap.continue)
			vim.keymap.set("n", "<F2>", dap.step_into)
			vim.keymap.set("n", "<F3>", dap.step_over)
			vim.keymap.set("n", "<F4>", dap.step_out)
			vim.keymap.set("n", "<F5>", dap.restart)
			vim.keymap.set("n", "<F11>", dap.run_last)
			vim.keymap.set("n", "<F12>", dapview.toggle)
		end,
	},
}
