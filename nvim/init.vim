set nocompatible              " be iMproved, required
filetype off                  " required

set spelllang=en
set cmdheight=2

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
set rtp+=~/.config/

call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'
Plugin 'tpope/vim-surround'
Plugin 'nvie/vim-flake8'
Plugin 'neovimhaskell/haskell-vim'
Plugin 'tpope/vim-commentary'
Plugin 'dense-analysis/ale'
Plugin 'christoomey/vim-titlecase'
Plugin 'preservim/vim-lexical' 
Plugin 'ionide/ionide-vim'
Plugin 'neoclide/coc.nvim'
Plugin 'rust-lang/rust.vim'
Plugin 'wfxr/minimap.vim'
Plugin 'airblade/vim-gitgutter'
Plugin 'nvim-tree/nvim-tree.lua'
Plugin 'nvim-tree/nvim-web-devicons'
Plugin 'nvim-telescope/telescope.nvim'
Plugin 'nvim-lua/plenary.nvim'
Plugin 'BurntSushi/ripgrep'

" All of your Plugins must be added before the following line
call vundle#end()            " required
:set number
:set relativenumber
" Sets the tabs length for insertion and deletion
:set expandtab
:set tabstop=4
:set shiftwidth=4
:set sts=4

:set foldmethod=indent
:set backspace=2
" Needed for homebrewsyntax on
syntax sync fromstart
syntax on
filetype plugin indent on
set splitbelow
set splitright
set langmap=jn,ke,il,nj,ek,li,mh,hm,JN,KE,IL,NJ,EK,LI,MH,HM
nnoremap j gj
nnoremap k gk
nnoremap ^ g^
" changes first character in line to first character in visual line
nnoremap $ g$
" changes last character in line to last character in visual line
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>
nnoremap <C-s> :setlocal spell!<CR>

let b:ale_linters = ['flake8', 'mypy', 'hlint', 'hls']
let g:ale_set_signs = 1

" Layouts
function! Colemak()
    execute "set langmap=jn,ke,il,nj,ek,li,JN,KE,IL,NJ,EK,LI"
endfunction
function! ColemakDH()
    execute "set langmap=jn,ke,il,nj,ek,li,mh,hm,JN,KE,IL,NJ,EK,LI,MH,HM"
endfunction
function! Qwerty()
    execute "set langmap="
endfunction
if $LAYOUT == "ColemakDH"
    call ColemakDH()
elseif $LAYOUT == "Qwerty"
    call Qwerty()
endif

nmap <silent> <leader>aj :ALENext<cr>
nmap <silent> <leader>ak :ALEPrevious<cr>

set clipboard+=unnamedplus

lua require('config')
lua require('nvim-tree').setup()
