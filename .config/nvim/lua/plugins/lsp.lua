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
				"rust",
				"zig",
				"json",
				"yaml",
				"toml",
				"bash",
				"latex",
				"html",
				"css",
				"javascript",
				"typescript",
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
					local ok, parser = pcall(vim.treesitter.get_parser, args.buf)
					if ok and parser then
						vim.treesitter.start(args.buf)
						if vim.treesitter.highlight then
							vim.treesitter.highlight.disable(args.buf)
						end
					else
						vim.bo[args.buf].indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
					end
				end,
			})
		end,
	},

	-- LSP
	{
		"neovim/nvim-lspconfig",
		dependencies = {
			{ "williamboman/mason.nvim", opts = { ensure_installed = { "google-java-format" } } },
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
								typeCheckingMode = "basic",
							},
						},
					},
				},
				clangd = {
					init_options = { fallbackFlags = { "-std=c++23" } },
				},
				hls = {},
				jdtls = {},
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

					local map = function(keys, fn, desc)
						vim.keymap.set("n", keys, fn, { buffer = args.buf, desc = desc })
					end

					map("gr", vim.lsp.buf.references, "[G]oto [R]eferences")
					map("gi", vim.lsp.buf.implementation, "[G]oto [I]mplementation")
					map("K", vim.lsp.buf.hover, "Hover documentation")
					map("<leader>rn", vim.lsp.buf.rename, "[R]e[n]ame")
					map("<leader>ca", vim.lsp.buf.code_action, "[C]ode [A]ction")
					map("<leader>ls", vim.lsp.buf.document_symbol, "[L]SP [S]ymbols")
					map("<leader>li", function()
						vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled({ bufnr = args.buf }), { bufnr = args.buf })
					end, "Toggle [I]nlay [H]ints")
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
				java = { "google-java-format" },
				lua = { "stylua" },
				ocaml = { "ocamlformat" },
				rust = { "rustfmt" },
				zig = { "zigfmt" },
			},
			formatters = {
				["google-java-format"] = {
					prepend_args = { "--aosp" },
				},
			},
		},
	},

	-- Completion
	{
		"hrsh7th/nvim-cmp",
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
			})
		end,
	},
}
