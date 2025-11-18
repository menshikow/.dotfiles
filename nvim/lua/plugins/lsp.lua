return {
    {
        "folke/lazydev.nvim",
        ft = "lua",
        opts = {
            library = {
                { path = '${3rd}/luv/library', words = { 'vim%.uv' } },
            },
        },
    }, {
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
                        typeCheckingMode = "off",
                    },
                },
            },
            clangd = {
                init_options = {
                    fallbackFlags = {
                        "-std=c++23",
                    },
                },
            },
            lua_ls = {},
            rust_analyzer = {},
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

        vim.diagnostic.config({
            float = {
                border = "rounded",
                source = "always",
                header = "",
                prefix = "",
            },

            signs = {
                text = {
                    [vim.diagnostic.severity.ERROR] = "",
                    [vim.diagnostic.severity.WARN] = "",
                    [vim.diagnostic.severity.HINT] = "",
                    [vim.diagnostic.severity.INFO] = "",
                },
            },

            underline = {
                severity = { min = vim.diagnostic.severity.HINT },
            },

            update_in_insert = false,
            severity_sort = true,
        })

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
    {
        "stevearc/conform.nvim",
        event = { "BufWritePre" },
        cmd = { "ConformInfo" },
        keys = {
            {
                "<leader>f",
                function()
                    require("conform").format { async = true, lsp_format = "fallback" }
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
            },
        },
    },
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
                    -- moving by suggestions
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
