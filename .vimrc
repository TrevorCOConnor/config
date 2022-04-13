set nocompatible              " be iMproved, required
filetype off                  " required

set spelllang=en

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'
Plugin 'tpope/vim-surround'
Plugin 'nvie/vim-flake8'
Plugin 'neovimhaskell/haskell-vim'
Plugin 'tpope/vim-commentary'
Plugin 'lervag/vimtex'
Plugin 'dense-analysis/ale'
Plugin 'christoomey/vim-titlecase'
Plugin 'preservim/vim-lexical' 

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
" To ignore plugin indent changes, instead use:
"filetype plugin on
"
" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal
"
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line

" inoremap ;; <Esc>
" Sets the line numbers
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
set splitbelow
set splitright
set langmap=jn,ke,il,nj,ek,li,JN,KE,IL,NJ,EK,LI
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

let b:ale_linters = ['flake8', 'mypy']
let b:ale_fixers = ['autoflake', 'trim_whitespace', 'add_blank_lines_for_python_control_statements', 'autopep8']

function! Colemak()
    execute "set langmap=jn,ke,il,nj,ek,li,JN,KE,IL,NJ,EK,LI"
endfunction
function! ColemakDH()
    execute "set langmap=jn,ke,il,nj,ek,li,mh,hm,JN,KE,IL,NJ,EK,LI,MH,HM"
endfunction
function! Qwerty()
    execute "set langmap="
endfunction

nmap <silent> <leader>aj :ALENext<cr>
nmap <silent> <leader>ak :ALEPrevious<cr>
