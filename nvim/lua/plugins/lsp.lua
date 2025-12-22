return {
	-- 1. LAZYDEV
	{
		"folke/lazydev.nvim",
		ft = "lua",
		opts = {
			library = {
				{ path = "${3rd}/luv/library", words = { "vim%.uv" } },
			},
		},
	},

	-- 2. TREESITTER
	{
		"nvim-treesitter/nvim-treesitter",
		build = ":TSUpdate",
		lazy = false,
		config = function()
			-- Force clang für macOS
			require("nvim-treesitter.install").compilers = { "clang" }

			require("nvim-treesitter.configs").setup({
				ensure_installed = { "c", "lua", "vim", "vimdoc", "query", "markdown", "cpp", "python" },
				modules = {},
				ignore_install = {},
				sync_install = false,
				auto_install = false,
				highlight = {
					enable = true,
					additional_vim_regex_highlighting = false,
				},
			})
		end,
	},
	{
		"nvim-treesitter/nvim-treesitter-textobjects",
		dependencies = { "nvim-treesitter/nvim-treesitter" },
	},

	-- 3. VIMTEX
	{
		"lervag/vimtex",
		lazy = false,
		init = function()
			-- Use Skim on macOS
			vim.g.vimtex_view_method = "skim"
			-- Optional: Disable concealing (hiding) of syntax
			vim.g.vimtex_syntax_conceal = { math_bounds = 0 }
		end,
	},

	-- 4. LSP (Language Servers)
	{
		"neovim/nvim-lspconfig",
		dependencies = {
			{ "williamboman/mason.nvim", opts = {} },
			"williamboman/mason-lspconfig.nvim",
			"hrsh7th/cmp-nvim-lsp",
		},
		config = function()
			local capabilities = require("cmp_nvim_lsp").default_capabilities()
			local servers = {
				basedpyright = {
					settings = {
						basedpyright = { typeCheckingMode = "basic" },
					},
				},
				clangd = {
					init_options = { fallbackFlags = { "-std=c++23" } },
				},
				lua_ls = {},
				rust_analyzer = {},
				zls = {},
				texlab = {},
			}
			local ensure_installed = vim.tbl_keys(servers or {})
			require("mason-lspconfig").setup({
				ensure_installed = ensure_installed,
				automatic_installation = false,
				handlers = {
					function(server_name)
						local server = servers[server_name] or {}
						server.capabilities = vim.tbl_deep_extend("force", {}, capabilities, server.capabilities or {})
						require("lspconfig")[server_name].setup(server)
					end,
				},
			})

			-- DIAGNOSTICS UI
			vim.diagnostic.config({
				virtual_text = false,
				float = { border = "rounded", source = true },
				signs = {
					text = {
						[vim.diagnostic.severity.ERROR] = "",
						[vim.diagnostic.severity.WARN] = "",
						[vim.diagnostic.severity.HINT] = "",
						[vim.diagnostic.severity.INFO] = "",
					},
				},
				underline = { severity = { min = vim.diagnostic.severity.HINT } },
				update_in_insert = false,
				severity_sort = true,
			})

			-- Underlines for diagnostics
			for _, group in ipairs({
				"DiagnosticUnderlineError",
				"DiagnosticUnderlineWarn",
				"DiagnosticUnderlineInfo",
				"DiagnosticUnderlineHint",
			}) do
				vim.api.nvim_set_hl(0, group, { underline = true, undercurl = false })
			end
		end,
	},

	-- 5. CONFORM (Formatting)
	{
		"stevearc/conform.nvim",
		event = { "BufWritePre" },
		cmd = { "ConformInfo" },
		keys = {
			{
				"<leader>f",
				function()
					require("conform").format({ async = true, lsp_format = "fallback" })
				end,
				mode = "",
			},
		},
		opts = {
			notify_on_error = false,
			format_on_save = {
				timeout_ms = 500,
				lsp_format = "fallback",
			},
			formatters_by_ft = {
				c = { "clang_format" },
				cpp = { "clang_format" },
				python = { "ruff_format" },
				javascript = { "prettier" },
				typescript = { "prettier" },
				javascriptreact = { "prettier" },
				typescriptreact = { "prettier" },
				css = { "prettier" },
				html = { "prettier" },
				json = { "prettier" },
				lua = { "stylua" },
				rust = { "rustfmt" },
				zig = { "zigfmt" },
				tex = { "latexindent" }, -- LaTeX Formatter
			},
		},
	},

	-- 6. COMPLETION (CMP)
	{
		"hrsh7th/nvim-cmp",
		dependencies = { "L3MON4D3/LuaSnip" },
		config = function()
			local cmp = require("cmp")
			cmp.setup({
				sources = {
					{ name = "nvim_lsp" },
				},
				mapping = cmp.mapping.preset.insert({
					["<CR>"] = cmp.mapping.confirm({ select = false }),
					["<C-Space>"] = cmp.mapping.complete(),
					["<Tab>"] = cmp.mapping.select_next_item(),
					["<S-Tab>"] = cmp.mapping.select_prev_item(),
					["<Down>"] = cmp.mapping.select_next_item(),
					["<Up>"] = cmp.mapping.select_prev_item(),
				}),
				snippet = {
					expand = function(args)
						require("luasnip").lsp_expand(args.body)
					end,
				},
			})
		end,
	},
}
