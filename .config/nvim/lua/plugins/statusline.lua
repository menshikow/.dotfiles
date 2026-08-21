return {
	"nvim-lualine/lualine.nvim",
	dependencies = { "nvim-tree/nvim-web-devicons", "ThePrimeagen/harpoon" },
	config = function()
		local harpoon = require("harpoon")
		local function harpoon_component()
			local list = harpoon:list()
			local current = list:current()
			if not current then
				return ""
			end
			local idx = list:index_of(current)
			return string.format(" ⚓ %d/%d", idx, #list.items)
		end

		require("lualine").setup({
			options = {
				theme = "nord",
				component_separators = "",
				section_separators = "",
				globalstatus = true,
			},
			sections = {
				lualine_a = { "mode" },
				lualine_b = { "branch", "diff", "diagnostics" },
				lualine_c = { { "filename", path = 1 } },
				lualine_x = { harpoon_component, "encoding", "fileformat", "filetype" },
				lualine_y = { "progress" },
				lualine_z = { "location" },
			},
		})
	end,
}