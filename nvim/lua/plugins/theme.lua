return {
	"menshikow/vim-mono.nvim",
	lazy = false,
	priority = 1000,
	config = function()
		vim.cmd.colorscheme("vim-mono")
	end,
}
