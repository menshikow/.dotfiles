-- curosr
vim.opt.guicursor = {
    "n-v:block",
    "i:blinkwait300-blinkon500-blinkoff300-block",
    "r-cr:hor20",
}

-- clipboard
vim.opt.clipboard = "unnamedplus"

-- number/sign column
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.signcolumn = "yes"

-- cursorline
vim.opt.cursorline = true

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
