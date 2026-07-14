call plug#begin()

Plug 'jiangmiao/auto-pairs'
Plug 'tpope/vim-sensible'

call plug#end()

colorscheme default
set number

syntax on

set expandtab

set tabstop=2
set shiftwidth=2
set softtabstop=2

set autoindent
set smartindent
set clipboard=unnamedplus

highlight StatusLine   cterm=NONE ctermfg=white ctermbg=black gui=NONE
highlight StatusLineNC cterm=NONE ctermfg=gray  ctermbg=black gui=NONE
