local map = vim.keymap.set
local opts = { noremap = true, silent = true }

-- move selected lines up/down and reselect
map("v", "J", ":m '>+1<CR>gv=gv", opts)
map("v", "K", ":m '<-2<CR>gv=gv", opts)

-- keep cursor centered when joining / scrolling / searching
map("n", "J", "mzJ`z", opts)
map("n", "<C-d>", "<C-d>zz", opts)
map("n", "<C-u>", "<C-u>zz", opts)
map("n", "n", "nzzzv", opts)
map("n", "N", "Nzzzv", opts)

-- yank to system clipboard
map({ "n", "v" }, "<leader>y", '"+y', opts)
map("n", "<leader>Y", '"+Y', opts)

-- paste without overwriting the unnamed register
map("x", "<leader>p", [["_dP]], opts)

-- delete without yanking
map({ "n", "v" }, "<leader>d", [["_d]], opts)

-- Clear search highlights
map("n", "<Esc>", "<cmd>nohlsearch<CR>", opts)

-- diagnostic
map("n", "gl", vim.diagnostic.open_float, opts)
map("n", "[d", vim.diagnostic.goto_prev, opts)
map("n", "]d", vim.diagnostic.goto_next, opts)
map("n", "<leader>k", vim.diagnostic.goto_prev, opts)
map("n", "<leader>j", vim.diagnostic.goto_next, opts)

-- window splits
map("n", "<leader>sv", "<cmd>vsplit<CR>", opts)
map("n", "<leader>sh", "<cmd>split<CR>", opts)
map("n", "<leader>se", "<C-w>=", opts)
map("n", "<leader>sx", "<cmd>close<CR>", opts)
map("n", "<leader>so", "<cmd>only<CR>", opts)

-- move between split windows
map("n", "<C-h>", "<C-w>h", opts)
map("n", "<C-j>", "<C-w>j", opts)
map("n", "<C-k>", "<C-w>k", opts)
map("n", "<C-l>", "<C-w>l", opts)

-- resize split windows
map("n", "<leader>rh", "<cmd>vertical resize -2<CR>", opts)
map("n", "<leader>rl", "<cmd>vertical resize +2<CR>", opts)
map("n", "<leader>rj", "<cmd>resize +2<CR>", opts)
map("n", "<leader>rk", "<cmd>resize -2<CR>", opts)
