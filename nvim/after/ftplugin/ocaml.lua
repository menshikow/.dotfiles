local set = vim.opt_local

set.shiftwidth = 2

local ok, mappings = pcall(require, "ocaml.mappings")

if ok then
	vim.keymap.set("n", "<space>cp", mappings.dune_promote_file, { buffer = 0 })
	vim.keymap.set("n", "<space>cd", mappings.destruct, { buffer = 0 })
else
	vim.keymap.set("n", "<space>cp", vim.lsp.buf.code_action, { buffer = 0 })
	vim.keymap.set("n", "<space>cd", vim.lsp.buf.code_action, { buffer = 0 })
end
