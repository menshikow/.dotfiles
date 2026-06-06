return {
	"tjdevries/express_line.nvim",

	config = function()
		local el = require("el")
		local builtin = require("el.builtin")
		local modes = require("el.data").modes
		local sections = require("el.sections")

		local plain_mode = function()
			local mode = vim.api.nvim_get_mode().mode
			local display = (modes[mode] and modes[mode][1]) or mode
			return string.format("[%s]", display)
		end

		vim.opt.laststatus = 2

		vim.api.nvim_set_hl(0, "StatusLine", {
			bg = "NONE",
			reverse = false,
		})

		vim.api.nvim_set_hl(0, "StatusLineNC", {
			bg = "NONE",
			reverse = false,
		})

		el.setup({
			generator = function()
				return {
					plain_mode,
					sections.split,
					sections.split,
					" ",
					builtin.file_relative,
					sections.split,
					" ",
					sections.split,
					"[",
					builtin.line,
					" : ",
					builtin.column,
					"]",
					builtin.filetype,
				}
			end,
		})

		vim.api.nvim_create_autocmd("FileType", {
			pattern = "TelescopePrompt",
			callback = function()
				vim.opt_local.statusline = ""
			end,
		})
	end,
}

-- return {
-- 	"itchyny/lightline.vim",
-- 	lazy = false,
-- 	config = function()
-- 		vim.o.showmode = false
-- 		vim.g.lightline = {
-- 			active = {
-- 				left = {
-- 					{ "mode", "paste" },
-- 					{ "readonly", "filename", "modified" },
-- 				},
-- 				right = {
-- 					{ "lineinfo" },
-- 					{ "percent" },
-- 					{ "fileencoding", "filetype" },
-- 				},
-- 			},
-- 			component_function = {
-- 				filename = "LightlineFilename",
-- 			},
-- 		}
--
-- 		function LightlineFilenameInLua(opts)
-- 			if vim.fn.expand("%:t") == "" then
-- 				return "[No Name]"
-- 			else
-- 				return vim.fn.getreg("%")
-- 			end
-- 		end
--
-- 		vim.api.nvim_exec(
-- 			[[
-- 				function! g:LightlineFilename()
-- 					return v:lua.LightlineFilenameInLua()
-- 				endfunction
-- 				]],
-- 			true
-- 		)
-- 	end,
-- }
