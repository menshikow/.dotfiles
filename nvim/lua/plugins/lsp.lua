return {
	{
		"folke/lazydev.nvim",
		ft = "lua",
		opts = {
			library = {
				{ path = "${3rd}/luv/library", words = { "vim%.uv" } },
			},
		},
	},

	{
		"nvim-treesitter/nvim-treesitter",
		build = ":TSUpdate",
		lazy = false,
		config = function()
			local parsers = {
				"c",
				"cpp",
				"haskell",
				"lua",
				"vim",
				"vimdoc",
				"query",
				"markdown",
				"python",
				"ocaml",
				"ocaml_interface",
				"java",
			}

			local installed = {} --- @type table<string, boolean>
			for _, p in ipairs(vim.api.nvim_get_runtime_file("parser/*", false)) do
				installed[vim.fn.fnamemodify(p, ":t")] = true
			end

			local missing = {} --- @type string[]
			for _, lang in ipairs(parsers) do
				if not installed[lang .. ".so"] then
					table.insert(missing, lang)
				end
			end

			if #missing > 0 then
				require("nvim-treesitter").install(missing)
			end

			vim.api.nvim_create_autocmd("FileType", {
				callback = function(args)
					local ok = pcall(vim.treesitter.get_parser, args.buf)
					if not ok then
						vim.bo[args.buf].indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
					end
				end,
			})
		end,
	},

	{
		"nvim-treesitter/nvim-treesitter-textobjects",
		branch = "main",
		dependencies = { "nvim-treesitter/nvim-treesitter" },
	},

	-- LSP
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
						basedpyright = {
							analysis = {
								typeCheckingMode = "off",
							},
						},
					},
				},
				clangd = {
					init_options = { fallbackFlags = { "-std=c++23" } },
				},
				hls = {},
				lua_ls = {},
				ocamllsp = {},
				rust_analyzer = {
					settings = {
						["rust-analyzer"] = {
							cargo = {
								features = "all",
							},
							checkOnSave = {
								enable = true,
							},
							check = {
								command = "clippy",
							},
							imports = {
								group = {
									enable = false,
								},
							},
							completion = {
								postfix = {
									enable = false,
								},
							},
						},
					},
				},
				zls = {},
				racket_langserver = {},

				texlab = {
					settings = {
						texlab = {
							build = {
								executable = "latexmk",
								args = {
									"-pdf",
									"-interaction=nonstopmode",
									"-synctex=1",
									"%f",
								},
							},
						},
					},
				},
			}

			-- mason

			require("mason-lspconfig").setup({
				ensure_installed = vim.tbl_keys(servers),
				automatic_installation = false,
				handlers = {
					function(server_name)
						local server = servers[server_name] or {}

						server.capabilities = vim.tbl_deep_extend("force", {}, capabilities, server.capabilities or {})

						require("lspconfig")[server_name].setup(server)
					end,
				},
			})

			vim.api.nvim_create_autocmd("LspAttach", {
				callback = function(args)
					local client = vim.lsp.get_client_by_id(args.data.client_id)
					if client then
						client.server_capabilities.semanticTokensProvider = nil
					end
				end,
			})

			vim.diagnostic.config({
				-- Change this to a table to enable and customize the inline text
				virtual_text = {
					spacing = 4,
					prefix = "●", -- Changes the bullet point before the text. You can use "~" or "■" too.
				},
				float = { border = "rounded", source = true },
				signs = {
					text = {
						[vim.diagnostic.severity.ERROR] = "E",
						[vim.diagnostic.severity.WARN] = "W",
						[vim.diagnostic.severity.HINT] = "H",
						[vim.diagnostic.severity.INFO] = "I",
					},
				},
				-- I recommend turning underline back on (true) for errors, but leaving it false if you hate the squiggly lines!
				underline = false,
				update_in_insert = false,
				severity_sort = true,
			})

			-- Force Red for inline errors
			vim.api.nvim_set_hl(0, "DiagnosticVirtualTextError", { fg = "#cc241d", italic = true })

			-- Force White for inline hints
			vim.api.nvim_set_hl(0, "DiagnosticVirtualTextHint", { fg = "#ebdbb2", italic = true })

			-- Set Warning and Info colors just in case
			vim.api.nvim_set_hl(0, "DiagnosticVirtualTextWarn", { fg = "#d79921", italic = true })
			vim.api.nvim_set_hl(0, "DiagnosticVirtualTextInfo", { fg = "#83a598", italic = true })
		end,
	},

	-- Formatting
	{
		"stevearc/conform.nvim",
		event = { "BufWritePre" },
		cmd = { "ConformInfo" },
		keys = {
			{
				"<leader>f",
				function()
					require("conform").format({
						async = true,
						lsp_format = "fallback",
					})
				end,
				mode = { "n", "v" }, -- FIXED
			},
		},
		opts = {
			notify_on_error = true,
			format_on_save = {
				timeout_ms = nil,
			},
			formatters_by_ft = {
				c = { "clang_format" },
				cpp = { "clang_format" },
				haskell = { "ormolu" },
				python = { "ruff_format" },
				javascript = { "prettier" },
				typescript = { "prettier" },
				javascriptreact = { "prettier" },
				typescriptreact = { "prettier" },
				css = { "prettier" },
				html = { "prettier" },
				json = { "prettier" },
				lua = { "stylua" },
				ocaml = { "ocamlformat" },
				rust = { "rustfmt" },
				zig = { "zigfmt" },
			},
		},
	},

	-- Completion
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
					["<CR>"] = cmp.mapping.confirm({ select = true }),
					["<C-Space>"] = cmp.mapping.complete(),
					["<Tab>"] = cmp.mapping.select_next_item(),
					["<S-Tab>"] = cmp.mapping.select_prev_item(),
					["<Down>"] = cmp.mapping.select_next_item(),
					["<Up>"] = cmp.mapping.select_prev_item(),
				}),

				snippet = {
					expand = function(args)
						local ok, luasnip = pcall(require, "luasnip")
						if ok then
							luasnip.lsp_expand(args.body)
						end
					end,
				},
			})
		end,
	},
}
