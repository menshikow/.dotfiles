return {
	{
	    "rose-pine/neovim",
	    name = "rose-pine",
	    priority = 1000,
	    config = function()
		require("rose-pine").setup({
		    variant = "main", -- "auto", "main", "moon", "dawn"
		    styles = {
			bold = true,
			italic = false,
			transparency = false,
		    },
		    highlight_groups = {
			-- make background pure black
			Normal = { bg = "#000000" },
			NormalNC = { bg = "#000000" },
			NormalFloat = { bg = "#000000" },
			FloatBorder = { bg = "#000000" },
			SignColumn = { bg = "#000000" },
			VertSplit = { bg = "#000000" },
			StatusLine = { bg = "#000000" },
			CursorLine = { bg = "#0a0a0a" },
			CursorLineNr = { bg = "#000000", fg = "#e0def4", bold = true },
			MatchParen = { bg = "#2f2f2f", bold = true },
			-- Git signs
			GitSignsAdd = { fg = "#82c13e" },
			GitSignsChange = { fg = "#d4902b" },
			GitSignsDelete = { fg = "#f10e38" },
		    },
		})
		vim.cmd.colorscheme "rose-pine"

		-- Ensure no other highlight overrides sneak in
		vim.api.nvim_set_hl(0, "Normal", { bg = "#000000" })
		vim.api.nvim_set_hl(0, "NormalNC", { bg = "#000000" })
	    end,
	}
}
