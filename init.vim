set nocompatible              " be iMproved, required
filetype off                  " required

set spelllang=en
set cmdheight=2

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'
Plugin 'preservim/nerdtree'
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

let b:ale_linters = ['flake8', 'mypy', 'pylint', 'hlint', 'hls']
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

" NerdTree
" Start NERDTree. If a file is specified, move the cursor to its window.
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * NERDTree | if argc() > 0 || exists("s:std_in") | wincmd p | endif
" Exit Vim if NERDTree is the only window remaining in the only tab.
autocmd BufEnter * if tabpagenr('$') == 1 && winnr('$') == 1 && exists('b:NERDTree') && b:NERDTree.isTabTree() | quit | endif

nmap <silent> <leader>aj :ALENext<cr>
nmap <silent> <leader>ak :ALEPrevious<cr>
