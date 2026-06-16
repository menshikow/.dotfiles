-- splits
vim.opt.splitbelow = true
vim.opt.splitright = true

vim.opt.list = false

-- smart search
vim.opt.ignorecase = true
vim.opt.smartcase = true

-- testing
vim.opt.iskeyword:remove("_")

-- clipboard
vim.opt.clipboard = "unnamedplus"

-- number/sign column
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.signcolumn = "yes"

-- cursorline
vim.opt.cursorline = false

-- line wrap
vim.opt.wrap = false

-- indent
vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true
vim.opt.smartindent = true

-- undo
vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.undodir = os.getenv("HOME") .. "/.vim/undodir"
vim.opt.undofile = true

-- colors
vim.opt.termguicolors = true
vim.cmd([[let &t_Cs = "\e[4:3m"]])
vim.cmd([[let &t_Ce = "\e[4:0m"]])
