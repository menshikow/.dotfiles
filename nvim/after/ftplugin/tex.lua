local set = vim.opt_local

set.textwidth = 80
set.colorcolumn = "81"
set.wrap = true
set.linebreak = true
set.spell = true
set.spelllang = "en"
set.spelloptions = { "camel" }

set.expandtab = false
set.shiftwidth = 2
set.tabstop = 2
set.softtabstop = 2

set.conceallevel = 1
set.concealcursor = "nvic"

local keymap = vim.keymap.set
keymap("n", "<localleader>ll", "<cmd>VimtexCompile<CR>", { buffer = true, silent = true, desc = "Vimtex: compile" })
keymap("n", "<localleader>lv", "<cmd>VimtexView<CR>", { buffer = true, silent = true, desc = "Vimtex: view (forward search)" })
keymap("n", "<localleader>lt", "<cmd>VimtexTocToggle<CR>", { buffer = true, silent = true, desc = "Vimtex: TOC" })
keymap("n", "<localleader>le", "<cmd>VimtexErrors<CR>", { buffer = true, silent = true, desc = "Vimtex: errors" })
keymap("n", "<localleader>lE", "<cmd>VimtexClean<CR>", { buffer = true, silent = true, desc = "Vimtex: clean aux" })
keymap("n", "<localleader>lo", "<cmd>VimtexCompileOutput<CR>", { buffer = true, silent = true, desc = "Vimtex: compile output" })
keymap("n", "<localleader>lm", "<cmd>VimtexImapsList<CR>", { buffer = true, silent = true, desc = "Vimtex: insert mappings" })
keymap("n", "<localleader>li", "<cmd>VimtexInfo<CR>", { buffer = true, silent = true, desc = "Vimtex: info" })
keymap({ "n", "x" }, "<localleader>$", "<cmd>VimtexDelim<CR>", { buffer = true, silent = true, desc = "Vimtex: change surroundings" })
keymap("n", "<localleader>lac", "<cmd>VimtexAutocloseBuf<CR>", { buffer = true, silent = true, desc = "Vimtex: autoclose buffer" })
