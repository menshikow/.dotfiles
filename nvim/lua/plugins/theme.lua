-- return {
-- 	"wincent/base16-nvim",
-- 	lazy = false,
-- 	priority = 1000,
-- 	config = function()
-- 		vim.cmd([[colorscheme gruvbox-dark-hard]])
-- 		vim.o.background = "dark"
-- 		vim.cmd([[hi Normal ctermbg=NONE]])
--
-- 		-- Less visible window separator
-- 		vim.api.nvim_set_hl(0, "WinSeparator", { fg = 1250067 })
--
-- 		-- Make comments more prominent -- they are important.
-- 		local bools = vim.api.nvim_get_hl(0, { name = "Boolean" })
-- 		vim.api.nvim_set_hl(0, "Comment", bools)
--
-- 		-- Make it clearly visible which argument we're at.
-- 		local marked = vim.api.nvim_get_hl(0, { name = "PMenu" })
-- 		vim.api.nvim_set_hl(
-- 			0,
-- 			"LspSignatureActiveParameter",
-- 			{ fg = marked.fg, bg = marked.bg, ctermfg = marked.ctermfg, ctermbg = marked.ctermbg, bold = true }
-- 		)
-- 	end,
-- }

return {
	"phha/zenburn.nvim",
	lazy = false,
	priority = 1000,
	config = function()
		vim.cmd.colorscheme("zenburn")
		vim.api.nvim_set_hl(0, "Normal", {
			bg = "#000000",
		})
	end,
}
