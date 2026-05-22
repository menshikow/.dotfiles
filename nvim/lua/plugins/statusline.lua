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

		el.setup({
			generator = function()
				return {
					plain_mode,
					" ",
					builtin.file_relative,
					" ",
					sections.split,
					"[",
					builtin.line,
					" : ",
					builtin.column,
					"]",
					"[",
					builtin.filetype,
					"]",
				}
			end,
		})
	end,
}
