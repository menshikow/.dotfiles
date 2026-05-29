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

		vim.api.nvim_set_hl(0, "StatusLine", { bg = "NONE", reverse = false })

		-- do the same for splits
		vim.api.nvim_set_hl(0, "StatusLineNC", { bg = "NONE", reverse = false })

		el.setup({
			generator = function(_, buffer)
				if vim.api.nvim_buf_get_option(buffer.bufnr, "buftype") == "prompt" then
					return { sections.split }
				end
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
	end,
}
