return {
	{
		"lervag/vimtex",
		ft = { "tex" },
		lazy = false,
		init = function()
			vim.g.vimtex_view_method = "sioyek"
			vim.g.vimtex_mappings_enabled = false
		end,
	},
}
