return {
	"lervag/vimtex",
	lazy = false,
	init = function()
		vim.g.vimtex_fold_enabled = 1
		vim.g.vimtex_quickfix_mode = 0
		vim.g.vimtex_compiler_latexmk_engines = { ["_"] = "-lualatex -shell-escape" }
		vim.g.vimtex_indent_on_ampersands = 0
		vim.g.vimtex_view_method = "sioyek"
		vim.g.matchup_override_vimtex = 1
		vim.opt.conceallevel = 0
		vim.opt.concealcursor = ""
		vim.g.latexindent_opt = "-m"

		vim.keymap.set("n", "<leader>ll", "<cmd>VimtexCompile<CR>")
		vim.keymap.set("n", "<leader>lv", "<cmd>VimtexView<CR>", { desc = "View PDF" })
		vim.keymap.set("n", "<leader>lc", "<cmd>VimtexClean<CR>", { desc = "Clean aux files" })
		vim.keymap.set("n", "<leader>le", "<cmd>VimtexErrors<CR>", { desc = "Show errors" })
		vim.keymap.set("n", "<leader>lt", "<cmd>VimtexTocOpen<CR>", { desc = "Table of contents" })

		vim.opt.wildignore:append({ ".aux", ".out", ".toc" })
	end,
	config = function()
		local api = vim.api

		api.nvim_set_hl(0, "texMathDelimZoneLI", { link = "GruvboxOrange" })
		api.nvim_set_hl(0, "texMathDelimZoneLD", { link = "GruvboxOrange" })

		local has_autopairs, npairs = pcall(require, "nvim-autopairs")
		if has_autopairs then
			local rule = require("nvim-autopairs.rule")
			local cond = require("nvim-autopairs.conds")

			-- Add a rule for matching inline math dollar signs
			npairs.add_rules({
				rule("$", "$", { "tex", "latex" }):with_cr(cond.none()),
			})

			-- Prevent specific pairs from auto-triggering within LaTeX files
			local backtick_rules = npairs.get_rules("`")
			if backtick_rules and backtick_rules[1] then
				backtick_rules[1].not_filetypes = { "tex", "latex" }
			end

			local quote_rules = npairs.get_rules("'")
			if quote_rules and quote_rules[1] then
				quote_rules[1].not_filetypes = { "tex", "latex", "rust" }
			end
		end

		api.nvim_create_autocmd("FileType", {
			pattern = { "tex", "latex" },
			callback = function()
				vim.bo.syntax = "on"

				if vim.g.vimtex_fold_enabled == 1 then
					vim.opt_local.foldmethod = "expr"
					vim.opt_local.foldexpr = "vimtex#fold#expr(v:lnum)"
				else
					vim.opt_local.foldmethod = "manual"
					vim.opt_local.foldexpr = ""
				end
			end,
		})
	end,
}
