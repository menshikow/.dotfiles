local map = vim.keymap.set
local opts = { noremap = true, silent = true }

-- === Movement ===
-- Move selected lines up/down and reselect
map("v", "J", ":m '>+1<CR>gv=gv", opts)
map("v", "K", ":m '<-2<CR>gv=gv", opts)

-- Keep cursor centered when joining / scrolling / searching
map("n", "J", "mzJ`z", opts)
map("n", "<C-d>", "<C-d>zz", opts)
map("n", "<C-u>", "<C-u>zz", opts)
map("n", "n", "nzzzv", opts)
map("n", "N", "Nzzzv", opts)

-- === Clipboard / Deletion ===
-- Yank to system clipboard
map({ "n", "v" }, "<leader>y", '"+y', opts)
map("n", "<leader>Y", '"+Y', opts)

-- Paste without overwriting the unnamed register
map("x", "<leader>p", [["_dP]], opts)

-- Delete without yanking
map({ "n", "v" }, "<leader>d", [["_d]], opts)

-- === UX / Misc ===
-- Clear search highlights
map("n", "<Esc>", "<cmd>nohlsearch<CR>", opts)

-- Presenter mode toggle (you could make this a toggle)
map("n", "<leader>;;", function()
  vim.o.background = "light"
  vim.cmd.colorscheme("retrobox")
end, opts)

-- === Diagnostics ===
map("n", "gl", vim.diagnostic.open_float, opts)
map("n", "[d", vim.diagnostic.goto_prev, opts)
map("n", "]d", vim.diagnostic.goto_next, opts)

