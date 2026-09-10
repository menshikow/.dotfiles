-- always set leader first!

vim.keymap.set("n", "<Space>", "<Nop>", { silent = true })
vim.g.mapleader = " "

-------------------------------------------------------------------------------
--
-- preferences
--
-------------------------------------------------------------------------------
-- never ever folding
vim.opt.foldenable = false
vim.opt.foldmethod = "manual"

vim.opt.foldlevelstart = 99

-- very basic "continue indent" mode (autoindent) is always on in neovim
-- could try smartindent/cindent, but meh.
-- vim.opt.cindent = true
-- XXX
-- vim.opt.cmdheight = 2
-- vim.opt.completeopt = 'menuone,noinsert,noselect'
-- not setting updatedtime because I use K to manually trigger hover effects
-- and lowering it also changes how frequently files are written to swap.
-- vim.opt.updatetime = 300
-- if key combos seem to be "lagging"
-- http://stackoverflow.com/questions/2158516/delay-before-o-opens-a-new-line
-- vim.opt.timeoutlen = 300
-- keep more context on screen while scrolling
vim.opt.scrolloff = 2

-- never show me line breaks if they're not there
vim.opt.wrap = false

-- always draw sign column. prevents buffer moving when adding/deleting sign
vim.opt.signcolumn = "yes"

-- sweet sweet relative line numbers
vim.opt.relativenumber = true

-- and show the absolute line number for the current line
vim.opt.number = true

-- keep current content top + left when splitting
vim.opt.splitright = true
vim.opt.splitbelow = true

-- infinite undo!
-- NOTE: ends up in ~/.local/state/nvim/undo/
vim.opt.undofile = true

--" Decent wildmenu
-- in completion, when there is more than one match,
-- list all matches, and only complete to longest common match
vim.opt.wildmode = "list:longest"

-- when opening a file with a command (like :e),
-- don't suggest files like there:
vim.opt.wildignore = ".hg,.svn,*~,*.png,*.jpg,*.gif,*.min.js,*.swp,*.o,vendor,dist,_site"

-- fuck tabs
vim.opt.shiftwidth = 8
vim.opt.softtabstop = 8
vim.opt.tabstop = 8
vim.opt.expandtab = true

-- case-insensitive search/replace
vim.opt.ignorecase = true

-- unless uppercase in search term
vim.opt.smartcase = true

-- never ever make my terminal beep
vim.opt.vb = true

-- more useful diffs (nvim -d)
--- by ignoring whitespace
vim.opt.diffopt:append("iwhite")

--- and using a smarter algorithm
--- https://vimways.org/2018/the-power-of-diff/
--- https://stackoverflow.com/questions/32365271/whats-the-difference-between-git-diff-patience-and-git-diff-histogram
--- https://luppeng.wordpress.com/2020/10/10/when-to-use-each-of-the-git-diff-algorithms/
vim.opt.diffopt:append("algorithm:histogram")
vim.opt.diffopt:append("indent-heuristic")

-- show a column at 80 characters as a guide for long lines
-- vim.opt.colorcolumn = '80'
--- vim.api.nvim_create_autocmd("Filetype", { pattern = "rust", command = "set colorcolumn=100" })
vim.opt.colorcolumn = ""

-- show more hidden characters
-- also, show tabs nicer
vim.opt.listchars = "tab:^ ,nbsp:¬,extends:»,precedes:«,trail:•"

-- things ive added myself
vim.opt.clipboard = "unnamedplus"
vim.opt.iskeyword:remove("_")

-------------------------------------------------------------------------------
--
-- hotkeys
--
-------------------------------------------------------------------------------

-- window splits
vim.keymap.set("n", "<C-x>3", "<cmd>vsplit<CR>")

vim.keymap.set("n", "<C-x>2", "<cmd>split<CR>")
vim.keymap.set("n", "<leader>sv", "<cmd>vsplit<CR>")
vim.keymap.set("n", "<leader>sh", "<cmd>split<CR>")

vim.keymap.set("n", "<leader>se", "<C-w>=")
vim.keymap.set("n", "<leader>sx", "<cmd>close<CR>")
vim.keymap.set("n", "<leader>so", "<cmd>only<CR>")
-- quick-save
vim.keymap.set("n", "<leader>w", "<cmd>w<cr>")
-- make missing : less annoying
vim.keymap.set("n", ";", ":")
-- cntrl+h to stop searching
vim.keymap.set("v", "<C-h>", "<cmd>nohlsearch<cr>")
vim.keymap.set("n", "<C-h>", "<cmd>nohlsearch<cr>")
-- jump to start and end of line using the home row keys
vim.keymap.set("", "H", "^")
vim.keymap.set("", "L", "$")
-- <leader><leader> toggles between buffers
vim.keymap.set("n", "<leader><leader>", "<c-^>")
-- <leader>, shows/hides hidden characters
vim.keymap.set("n", "<leader>,", ":set invlist<cr>")
-- always center search results
vim.keymap.set("n", "n", "nzz", { silent = true })

vim.keymap.set("n", "N", "Nzz", { silent = true })
vim.keymap.set("n", "*", "*zz", { silent = true })
vim.keymap.set("n", "#", "#zz", { silent = true })
vim.keymap.set("n", "g*", "g*zz", { silent = true })
-- "very magic" (less escaping needed) regexes by default
vim.keymap.set("n", "?", "?\\v")
vim.keymap.set("n", "/", "/\\v")
vim.keymap.set("c", "%s/", "%sm/")
-- open new file adjacent to current file
vim.keymap.set("n", "<leader>o", ':e <C-R>=expand("%:p:h") . "/" <cr>')
-- more detailed diagnostics messages
vim.keymap.set("n", "gl", vim.diagnostic.open_float, { noremap = true, silent = true })
-- let the left and right arrows be useful: they can switch buffers
vim.keymap.set("n", "<left>", ":bp<cr>")
vim.keymap.set("n", "<right>", ":bn<cr>")
-- make j and k move by visual line, not actual line, when text is soft-wrapped
vim.keymap.set("n", "j", "gj")
vim.keymap.set("n", "k", "gk")
-- handy keymap for replacing up to next _ (like in variable names)
vim.keymap.set("n", "<leader>m", "ct_")
-- F1 is pretty close to Esc, so you probably meant Esc
vim.keymap.set("", "<F1>", "<Esc>")
vim.keymap.set("i", "<F1>", "<Esc>")


-------------------------------------------------------------------------------
--
-- configuring diagnostics
--
-------------------------------------------------------------------------------
-- Allow virtual text
vim.diagnostic.config({ virtual_text = true, virtual_lines = false })


-------------------------------------------------------------------------------
--
-- autocommands
--
-------------------------------------------------------------------------------

-- highlight yanked text
vim.api.nvim_create_autocmd("TextYankPost", {
        pattern = "*",
        command = "silent! lua vim.highlight.on_yank({ timeout = 500 })",
})
-- jump to last edit position on opening file
vim.api.nvim_create_autocmd("BufReadPost", {
        pattern = "*",
        callback = function(ev)
                if vim.fn.line("'\"") > 1 and vim.fn.line("'\"") <= vim.fn.line("$") then
                        -- except for in git commit messages
                        -- https://stackoverflow.com/questions/31449496/vim-ignore-specifc-file-in-autocommand
                        if not vim.fn.expand("%:p"):find(".git", 1, true) then
                                vim.cmd('exe "normal! g\'\\""')
                        end
                end
        end,
})
-- prevent accidental writes to buffers that shouldn't be edited
vim.api.nvim_create_autocmd("BufRead", { pattern = "*.orig", command = "set readonly" })
vim.api.nvim_create_autocmd("BufRead", { pattern = "*.pacnew", command = "set readonly" })
-- leave paste mode when leaving insert mode (if it was on)
vim.api.nvim_create_autocmd("InsertLeave", { pattern = "*", command = "set nopaste" })
-- help filetype detection (add as needed)

--vim.api.nvim_create_autocmd('BufRead', { pattern = '*.ext', command = 'set filetype=someft' })
-- correctly classify mutt buffers
local email = vim.api.nvim_create_augroup("email", { clear = true })

vim.api.nvim_create_autocmd({ "BufNewFile", "BufRead" }, {
        pattern = "/tmp/mutt*",
        group = email,
        command = "setfiletype mail",
})
-- also, produce "flowed text" wrapping
-- https://brianbuccola.com/line-breaks-in-mutt-and-vim/
vim.api.nvim_create_autocmd("Filetype", {
        pattern = "mail",
        group = email,
        command = "setlocal formatoptions+=w",
})
-- shorter columns in text because it reads better that way
-- local text = vim.api.nvim_create_augroup("text", { clear = true })
-- for _, pat in ipairs({ "text", "markdown", "mail", "gitcommit" }) do
-- 	vim.api.nvim_create_autocmd("Filetype", {
-- 		pattern = pat,
-- 		group = text,
-- 		command = "setlocal spell tw=72 colorcolumn=",
-- 	})
-- end
--- tex has so much syntax that a little wider is ok
vim.api.nvim_create_autocmd("Filetype", {
        pattern = "tex",
        group = text,
        command = "setlocal spell tw=80 colorcolumn=81",
})

-------------------------------------------------------------------------------
--
-- plugin configuration
--
-------------------------------------------------------------------------------
-- first, grab the manager
-- https://github.com/folke/lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
        vim.fn.system({

                "git",
                "clone",
                "--filter=blob:none",
                "https://github.com/folke/lazy.nvim.git",
                "--branch=stable", -- latest stable release
                lazypath,
        })
end

vim.opt.rtp:prepend(lazypath)
-- then, setup!
require("lazy").setup({
        -- main color scheme
        {
        	"wincent/base16-nvim",
        	lazy = false, -- load at start
        	priority = 1000, -- load first
        	config = function()
        		vim.cmd([[colorscheme gruvbox-dark-hard]])
        		vim.o.background = "dark"
        		vim.cmd([[hi Normal ctermbg=NONE]])
        		-- Less visible window separator
        		vim.api.nvim_set_hl(0, "WinSeparator", { fg = 1250067 })
        		-- Make comments more prominent -- they are important.

        		local bools = vim.api.nvim_get_hl(0, { name = "Boolean" })
        		vim.api.nvim_set_hl(0, "Comment", bools)
        		-- Make it clearly visible which argument we're at.

        		local marked = vim.api.nvim_get_hl(0, { name = "PMenu" })
        		vim.api.nvim_set_hl(
        			0,
        			"LspSignatureActiveParameter",
        			{ fg = marked.fg, bg = marked.bg, ctermfg = marked.ctermfg, ctermbg = marked.ctermbg, bold = true }
        		)
        		-- XXX
        		-- Would be nice to customize the highlighting of warnings and the like to make
        		-- them less glaring. But alas

        		-- https://github.com/nvim-lua/lsp_extensions.nvim/issues/21
        		-- call Base16hi("CocHintSign", g:base16_gui03, "", g:base16_cterm03, "", "", "")
        	end,

        },
        -- {
        --         "RRethy/nvim-base16",
        --         config = function()
        --                 require("base16-colorscheme").setup({
        --                         base00 = "#20201d",
        --                         base01 = "#292824",
        --                         base02 = "#6e6b5e",
        --                         base03 = "#7d7a68",
        --                         base04 = "#999580",
        --                         base05 = "#a6a28c",
        --                         base06 = "#e8e4cf",
        --                         base07 = "#fefbec",
        --                         base08 = "#d43552",
        --                         base09 = "#b65611",
        --                         base0A = "#ae9513",
        --                         base0B = "#60ac39",
        --                         base0C = "#1ead8f",
        --                         base0D = "#6684e1",
        --                         base0E = "#b854d4",
        --                         base0F = "#b46958",
        --                 })
        --
        --                 -- disable italics
        --                 vim.g.base16_italicize_comments = 0
        --
        --                 -- more readble comments, they are important
        --                 local comment_hl = { italic = false, fg = "#c6c3b5" }
        --                 vim.api.nvim_set_hl(0, "Comment", comment_hl)
        --                 vim.api.nvim_set_hl(0, "@comment", comment_hl)
        --         end,
        -- },

        -- nice bar at the bottom
        -- {
        -- 	"itchyny/lightline.vim",
        -- 	lazy = false, -- also load at start since it's UI
        -- 	config = function()
        -- 		-- no need to also show mode in cmd line when we have bar
        -- 		vim.o.showmode = false
        -- 		vim.g.lightline = {
        --
        -- 			active = {
        -- 				left = {
        -- 					{ "mode", "paste" },
        -- 					{ "readonly", "filename", "modified" },
        -- 				},
        -- 				right = {
        -- 					{ "lineinfo" },
        -- 					{ "percent" },
        -- 					{ "fileencoding", "filetype" },
        --
        -- 				},
        -- 			},
        -- 			component_function = {
        -- 				filename = "LightlineFilename",
        -- 			},
        -- 		}
        -- 		function LightlineFilenameInLua(opts)
        -- 			if vim.fn.expand("%:t") == "" then
        -- 				return "[No Name]"
        --                                else	
        -- 				return vim.fn.getreg("%")
        -- 			end
        -- 		end
        -- 		-- https://github.com/itchyny/lightline.vim/issues/657
        -- 		vim.api.nvim_exec(
        -- 			[[
        -- 			function! g:LightlineFilename()
        -- 				return v:lua.LightlineFilenameInLua()
        -- 			endfunction
        -- 			]],
        -- 			true
        -- 		)
        -- 	end,
        -- },
        {
                "nvim-lualine/lualine.nvim",
                opts = {
                        options = {
                                icons_enabled = false,
                                component_separators = "",
                                section_separators = "",
                        },
                },
        },
        -- better %
        {
                "andymass/vim-matchup",

                config = function()
                        vim.g.matchup_matchparen_offscreen = { method = "popup" }
                end,

        },
        -- option to center the editor
        {
                "shortcuts/no-neck-pain.nvim",
                version = "*",
                opts = {

                        mappings = {
                                enabled = true,
                                toggle = false,
                                toggleLeftSide = false,
                                toggleRightSide = false,
                                widthUp = false,
                                widthDown = false,
                                scratchPad = false,
                        },
                },
                config = function()
                        vim.keymap.set("", "<leader>t", function()
                                vim.cmd([[
					:NoNeckPain
					:set formatoptions-=tc linebreak tw=0 cc=0 wrap wm=20 noautoindent nocindent nosmartindent indentkeys=
				]])
                                -- make 0, ^ and $ behave better in wrapped text
                                vim.keymap.set("n", "0", "g0")
                                vim.keymap.set("n", "$", "g$")
                                vim.keymap.set("n", "^", "g^")
                        end)
                end,
        },
        {
                "lewis6991/gitsigns.nvim",
                opts = {},
        },
        -- :G status/diff/blame/commit, fills the gap left by dropping
        -- telescope's git_commits/git_branches pickers
        "tpope/vim-fugitive",
        -- respects a repo's .editorconfig, overriding your global
        -- tab/indent settings on a per-project basis when one is present
        "gpanders/editorconfig.nvim",
        -- lets `:e file.rs:42` (or `nvim file.rs:42` from the shell) jump
        -- straight to that line instead of opening at line 1
        "lewis6991/fileline.nvim",
        -- project-wide search-and-replace UI, pairs well with fff for
        -- find-then-replace-across-files workflows
        {
                "MagicDuck/grug-far.nvim",
                keys = {
                        { "<leader>fR", "<cmd>GrugFar<CR>", desc = "[F]ind/[R]eplace across files" },
                },
                opts = {},
        },
        {
                "windwp/nvim-autopairs",
                event = "InsertEnter",
                config = true,
        },
        -- surround-text-object editing: ys{motion}{char} to add, cs{old}{new}
        -- to change, ds{char} to delete a surrounding pair
        {
                "kylechui/nvim-surround",
                version = "*",
                event = "VeryLazy",
                opts = {},
        },
        -- shows marks (m{a-z}) in the sign column instead of leaving them invisible
        {
                "chentoast/marks.nvim",
                event = "VeryLazy",
                opts = {},
        },
        {
                "stevearc/oil.nvim",
                config = function()
                        require("oil").setup({
                                columns = {},
                                default_file_explorer = true,
                                delete_to_trash = true,
                                skip_confirm_for_simple_edits = true,
                                view_options = {
                                        show_hidden = true,
                                        natural_order = true,

                                },
                        })

                        vim.keymap.set("n", "<leader>e", "<CMD>Oil<CR>", { desc = "Open file [E]xplorer" })
                end,
        },
        {
                "mg979/vim-visual-multi",
                branch = "master",
                init = function()
                        pcall(vim.keymap.del, "n", "<C-n>")
                        pcall(vim.keymap.del, "x", "<C-n>")


                        vim.g.VM_maps = {
                                ["Find Under"] = "<C-n>",
                                ["Find Subword Under"] = "<C-n>",
                                ["Visual All"] = "<C-e>", -- select all occurrences of visual selection
                        }
                end,
        },
        {
                "folke/todo-comments.nvim",
                dependencies = { "nvim-lua/plenary.nvim" },
                opts = {},
        },
        -- auto-cd to root of git project
        -- 'airblade/vim-rooter'
        {
                "notjedi/nvim-rooter.lua",
                config = function()
                        require("nvim-rooter").setup()
                end,
        },
        -- hop: EasyMotion-style jump-to-anywhere motions
        -- (smoka7/hop.nvim, not phaazon/hop.nvim -- the original is
        -- unmaintained, smoka7's fork is the actively developed continuation)
        {
                "smoka7/hop.nvim",
                version = "*",
                opts = { keys = "etovxqpdygfblzhckisuran" },
                config = function(_, opts)
                        require("hop").setup(opts)
                        local hop = require("hop")

                        -- jump to any visible 2-char sequence, either direction
                        vim.keymap.set("n", "s", hop.hint_char2, { desc = "Hop to char" })
                        vim.keymap.set("v", "s", hop.hint_char2, { desc = "Hop to char" })

                        vim.keymap.set("n", "<leader>jw", hop.hint_words, { desc = "[J]ump to [W]ord" })
                        vim.keymap.set("n", "<leader>jl", hop.hint_lines, { desc = "[J]ump to [L]ine" })
                        vim.keymap.set("n", "<leader>jp", hop.hint_patterns, { desc = "[J]ump to [P]attern" })
                end,
        },
        -- fff: fast fuzzy file finder / live grep, backed by a Rust index
        -- (devicons dependency dropped: no file-type icons in the picker)
        {
                "dmtrKovalenko/fff.nvim",
                build = function()
                        require("fff.download").download_or_build_binary()
                end,
                config = function()
                        require("fff").setup({
                                wrap_around = true,
                                prompt = "> ",
                                grep = {
                                        trim_whitespace = true,
                                },
                                layout = {
                                        prompt_position = "bottom",
                                },
                        })
                end,
        },
        -- fff-plus: buffer picker + git-status picker on top of fff's index
        -- (third-party extension, smaller/less battle-tested than fff.nvim itself)
        {
                "vinitkumar/fff-plus.nvim",
                dependencies = { "dmtrKovalenko/fff.nvim" },
                opts = { legacy_commands = false },
                config = function(_, opts)
                        require("fff_plus").setup(opts)
                        local fff = require("fff")
                        local plus = require("fff_plus")

                        -- Files & Search
                        vim.keymap.set("n", "<C-p>", fff.find_files, { desc = "Find files" })
                        vim.keymap.set("n", "<leader>ff", fff.find_files, { desc = "[F]ind [F]iles" })
                        vim.keymap.set("n", "<leader>fg", fff.live_grep, { desc = "[F]ind by [G]rep (live)" })
                        vim.keymap.set("n", "<leader>fw", fff.live_grep_under_cursor,
                                { desc = "[F]ind [W]ord under cursor" })

                        -- Buffers
                        vim.keymap.set("n", "<leader>;", plus.buffers, { desc = "Find buffers" })
                        vim.keymap.set("n", "<leader>fb", plus.buffers, { desc = "[F]ind [B]uffers" })

                        -- Complex Prompt/Grep Map
                        vim.keymap.set("n", "<leader>fa", function()
                                local dir = vim.fn.input("Search in directory: ", vim.fn.getcwd(), "dir")
                                if dir ~= "" then
                                        fff.find_files_in_dir(dir)
                                end
                        end, { desc = "[F]ind [A]nywhere (all files)" })

                        -- Neovim Config Search
                        vim.keymap.set("n", "<leader>sn", function()
                                fff.find_files_in_dir(vim.fn.stdpath("config"))
                        end, { desc = "[S]earch [N]eovim Config" })

                        -- Git Integration
                        vim.keymap.set("n", "<leader>gS", plus.git_status, { desc = "[G]it [S]tatus" })

                        -- gb was previously bound to <C-o> (native jump-back) alongside a
                        -- separate telescope git_branches map on the same key in the git
                        -- block below it, so the branches map never actually fired; kept
                        -- as native jump-back here since that's what was live before.
                        vim.keymap.set("n", "gb", "<C-o>", { desc = "Go back" })
                end,
        },
        -- which-key.nvim: group/mapping icons disabled below
        {
                "folke/which-key.nvim",
                event = "VeryLazy",
                opts = {
                        icons = { mappings = false },
                },
        },
        -- quick navigation
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

        {
                "folke/lazydev.nvim",
                ft = "lua",
                opts = {
                        library = {
                                { path = "${3rd}/luv/library", words = { "vim%.uv" } },

                        },
                },
        },

        -- built-in regex-based syntax highlighting (no treesitter)
        {
                dir = vim.fn.stdpath("config"), -- dummy dir so lazy.nvim accepts this as a spec
                name = "builtin-syntax",
                lazy = false,
                priority = 1000,
                config = function()
                        vim.cmd("syntax on")
                        vim.cmd("filetype plugin indent on")
                end,

        },
        -- extended highliting for c and cpp
        { "octol/vim-cpp-enhanced-highlight", ft = { "c", "cpp" } },
        -- LSP
        {
                "neovim/nvim-lspconfig",
                dependencies = {
                        -- mason.nvim: status icons swapped for plain ASCII below
                        {
                                "williamboman/mason.nvim",
                                opts = {
                                        ui = {
                                                icons = {
                                                        package_installed = "[x]",
                                                        package_pending = "[~]",
                                                        package_uninstalled = "[ ]",
                                                },
                                        },
                                },
                        },
                        "williamboman/mason-lspconfig.nvim",
                        "saghen/blink.cmp",
                },
                config = function()
                        local capabilities = require("blink.cmp").get_lsp_capabilities()

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
                                ruff = {},
                                clangd = {
                                        init_options = { fallbackFlags = { "-std=c++23" } },
                                },
                                hls = {},
                                jdtls = {},
                                lua_ls = {},
                                ocamllsp = {
                                        -- resolves via the active opam switch (including local/per-project
                                        -- switches) instead of hardcoding the "default" switch's binary

                                        cmd = { "opam", "exec", "--", "ocamllsp" },
                                },
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

                                bashls = {},
                                html = {},
                                cssls = {},
                                jsonls = {},
                                yamlls = {},

                                ts_ls = {},
                                svelte = {},
                                terraformls = {},
                                marksman = {},
                        }

                        -- mason

                        require("mason-lspconfig").setup({
                                ensure_installed = vim.tbl_filter(function(name)
                                        -- ocamllsp is installed via opam, not mason
                                        return name ~= "ocamllsp"
                                end, vim.tbl_keys(servers)),

                                automatic_installation = false,
                                handlers = {
                                        function(server_name)
                                                local server = servers[server_name] or {}


                                                server.capabilities = vim.tbl_deep_extend("force", {}, capabilities,
                                                        server.capabilities or {})


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

                                        map("gD", vim.lsp.buf.declaration, "[G]oto [D]eclaration")
                                        map("gd", vim.lsp.buf.definition, "[G]oto [D]efinition")
                                        map("gr", vim.lsp.buf.references, "[G]oto [R]eferences")
                                        map("gi", vim.lsp.buf.implementation, "[G]oto [I]mplementation")
                                        map("K", vim.lsp.buf.hover, "Hover documentation")
                                        map("<C-k>", vim.lsp.buf.signature_help, "Signature help")
                                        map("<leader>rn", vim.lsp.buf.rename, "[R]e[n]ame")
                                        map("<leader>ca", vim.lsp.buf.code_action, "[C]ode [A]ction")
                                        map("<leader>ls", vim.lsp.buf.document_symbol, "[L]SP [S]ymbols")
                                        map("<leader>q", vim.diagnostic.setloclist, "Diagnostics to loclist")
                                        map("<leader>li", function()
                                                vim.lsp.inlay_hint.enable(
                                                        not vim.lsp.inlay_hint.is_enabled({ bufnr = args.buf }),
                                                        { bufnr = args.buf }
                                                )
                                        end, "Toggle [I]nlay [H]ints")
                                end,
                        })

                        -- diagnostic signs switched from icon glyphs to plain letters
                        vim.diagnostic.config({
                                virtual_text = {
                                        spacing = 4,
                                        prefix = "-",
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
                                underline = false,
                                update_in_insert = false,

                                severity_sort = true,
                        })

                        vim.api.nvim_set_hl(0, "DiagnosticVirtualTextError", { fg = "#cc241d", italic = true })
                        vim.api.nvim_set_hl(0, "DiagnosticVirtualTextHint", { fg = "#ebdbb2", italic = true })
                        vim.api.nvim_set_hl(0, "DiagnosticVirtualTextWarn", { fg = "#d79921", italic = true })
                        vim.api.nvim_set_hl(0, "DiagnosticVirtualTextInfo", { fg = "#83a598", italic = true })
                end,

        },

        -- inline function signatures
        {
                "ray-x/lsp_signature.nvim",
                event = "VeryLazy",

                opts = {},
                config = function(_, opts)
                        require("lsp_signature").setup({

                                doc_lines = 0,
                                handler_opts = {
                                        border = "none",
                                },
                        })
                end,
        },


        -- lightbulb sign switched from the default icon glyph to a plain
        -- asterisk -- complements <leader>ca
        {
                "kosayoda/nvim-lightbulb",
                event = "LspAttach",
                opts = {
                        autocmd = { enabled = true },
                        sign = { text = "*" },
                },
        },
        -- inline Cargo.toml dependency versions + upgrade hints via rust-analyzer
        {
                "saecki/crates.nvim",
                event = "BufRead Cargo.toml",
                dependencies = { "nvim-lua/plenary.nvim" },
                opts = {
                        lsp = {
                                enabled = true,
                                actions = true,
                                completion = true,
                                hover = true,
                        },
                },
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
                                mode = { "n", "v" },
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
                                svelte = { "prettier" },
                                css = { "prettier" },
                                html = { "prettier" },
                                json = { "prettier" },
                                yaml = { "prettier" },

                                markdown = { "prettier" },
                                ocaml = { "ocamlformat" },
                                rust = { "rustfmt" },

                                zig = { "zigfmt" },
                                terraform = { "terraform_fmt" },
                        },
                },
        },

        -- Completion
        -- blink.cmp: menu draw columns trimmed to drop the kind_icon column,
        -- so completion entries show label + description only, no icons
        {
                "saghen/blink.cmp",
                dependencies = {
                        {
                                "L3MON4D3/LuaSnip",
                                build = "make install_jsregexp",
                                config = function()
                                        local luasnip = require("luasnip")
                                        require("luasnip.loaders.from_vscode").load({ paths = "~/.config/nvim/snippets" })
                                        luasnip.config.set_config({
                                                region_check_events = "InsertEnter",
                                                delete_check_events = "InsertLeave",
                                        })
                                        luasnip.config.setup({})
                                end,
                        },
                },
                -- use a release tag to download pre-built binaries
                version = "1.*",
                -- AND/OR build from source, requires nightly: https://rust-lang.github.io/rustup/concepts/channels.html#working-with-nightly-rust
                -- build = 'cargo build --release',
                ---@module 'blink.cmp'
                ---@type blink.cmp.Config
                opts = {
                        keymap = {
                                preset = "enter",
                                ["<Tab>"] = { "fallback" },
                                ["<S-Tab>"] = { "snippet_forward", "fallback" },
                        },
                        snippets = { preset = "luasnip" },
                        appearance = {
                                nerd_font_variant = "mono",
                        },
                        completion = {
                                documentation = { auto_show = true },
                                menu = {
                                        draw = {
                                                columns = { { "label", "label_description", gap = 1 } },
                                        },
                                },
                        },
                        sources = {
                                default = { "snippets", "lsp", "path", "buffer" },
                        },
                        fuzzy = { implementation = "prefer_rust_with_warning" },
                },
                opts_extend = { "sources.default" },
        },

        -- filetype plugins not covered by LSP alone
        {
                "lervag/vimtex",
                ft = { "tex" },
                lazy = false, -- vimtex should not be lazy-loaded
                init = function()
                        vim.g.vimtex_view_method = "zathura"
                        vim.g.vimtex_mappings_enabled = false
                end,
        },
        {
                "plasticboy/vim-markdown",
                ft = { "markdown" },
                dependencies = {
                        "godlygeek/tabular",
                },
                config = function()
                        vim.g.vim_markdown_folding_disabled = 1
                        vim.g.vim_markdown_frontmatter = 1
                        vim.g.vim_markdown_new_list_item_indent = 0
                        vim.g.vim_markdown_auto_insert_bullets = 0
                end,
        },
        "khaveesh/vim-fish-syntax",
        -- undotree
        {
                "mbbill/undotree",
                keys = {
                        { "<leader>u", "<cmd>UndotreeToggle<CR>", desc = "Toggle undo tree" },
                },
        },
        -- code stas
        { 'wakatime/vim-wakatime',            lazy = false },
})
