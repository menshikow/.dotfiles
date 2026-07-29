call plug#begin()

Plug 'jiangmiao/auto-pairs'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-commentary'
Plug 'mg979/vim-visual-multi'

call plug#end()

let mapleader = " "

nnoremap <leader>e :Ex<CR>

set noswapfile
set nobackup
set noundofile
set nowrap

colorscheme default
set relativenumber

syntax on

set expandtab

set tabstop=2
set shiftwidth=2
set softtabstop=2

set autoindent
set smartindent

if has('mac') || has('macunix')
  set clipboard=unnamed
else
  set clipboard=unnamedplus
endif

highlight StatusLine   cterm=NONE ctermfg=white ctermbg=black gui=NONE
highlight StatusLineNC cterm=NONE ctermfg=gray  ctermbg=black gui=NONE

" better search settings 
set ignorecase
set smartcase
set incsearch
set hlsearch

nnoremap <Esc> :nohlsearch<CR><Esc>


