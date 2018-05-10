set nocompatible              " be iMproved, required

" Plugins are defined here using plug-vim
call plug#begin('~/.config/nvim/bundle')

Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'guns/vim-clojure-highlight'
Plug 'SevereOverfl0w/vim-replant', { 'do': ':UpdateRemotePlugins' }
Plug 'luochen1990/rainbow'
Plug 'scrooloose/nerdtree'
Plug 'inside/vim-search-pulse'
Plug 'ayu-theme/ayu-vim'
Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'joshdick/onedark.vim'
Plug 'lifepillar/vim-solarized8'
Plug 'nvie/vim-flake8'
Plug 'pangloss/vim-javascript'
Plug 'alvan/vim-closetag'
Plug 'tpope/vim-commentary'
Plug 'terryma/vim-smooth-scroll'
Plug 'matze/vim-move'
Plug 'vim-airline/vim-airline'
Plug 'easymotion/vim-easymotion'
Plug 'tpope/vim-fireplace', { 'for': 'clojure' }
Plug 'jeffkreeftmeijer/vim-numbertoggle'
Plug 'tpope/vim-repeat'
Plug 'guns/vim-sexp'
Plug 'tpope/vim-sexp-mappings-for-regular-people'
Plug 'tpope/vim-surround'
Plug 'christoomey/vim-tmux-navigator'
Plug 'Valloric/YouCompleteMe', { 'do': './install.py --tern-completer --clang-completer' }
Plug 'jiangmiao/auto-pairs'
Plug 'ntpeters/vim-better-whitespace'
Plug 'mxw/vim-jsx'
Plug 'kshenoy/vim-signature'
Plug 'neovimhaskell/haskell-vim'
Plug 'mfukar/robotframework-vim'

call plug#end()

syntax enable
set synmaxcol=300
syntax sync minlines=256
set regexpengine=1

set termguicolors

set showcmd

set lazyredraw

" A selection of nice color schemes to alternate between

" let ayucolor="mirage"
" colorscheme dracula
" colorscheme monokai
" colorscheme onedark
colorscheme gruvbox
let g:gruvbox_contrast_dark = 'soft'
" colorscheme ayu
" colorscheme apprentice
filetype plugin indent on    " required

map <C-n> :NERDTreeToggle<CR>
let NERDTreeShowHidden=1

" let g:rainbow_active = 1 "0 if you want to enable it later via :RainbowToggle
set encoding=utf-8
set fileencoding=utf-8
set cursorline " - Valitun rivin korostus
set magic
" set showmatch
set smarttab
set shiftwidth=2
set tabstop=2
set expandtab
set nolinebreak
set nolist
set textwidth=0
set wrapmargin=0
set autoread
set ignorecase
set smartcase
set background=dark
set ai
set nu
set pastetoggle=<F2>
set mouse=
set backup
set swapfile

"" Text Wrapping
"set textwidth=79
set colorcolumn=80
set wrap
set nolist  " list disables linebreak

" Save temporary/backup files not in the local directory, but in your ~/.vim
" directory, to keep them out of git repos.
" But first mkdir backup, swap, and undo first to make this work
call system('mkdir ~/.vim')
call system('mkdir ~/.vim/backup')
call system('mkdir ~/.vim/swap')
set backupdir=~/.vim/backup//
set directory=~/.vim/swap//

" Keep undo history across sessions by storing it in a file
if has('persistent_undo')
    call system('mkdir ~/.vim/undo')
    set undodir=~/.vim/undo//
    set undofile
    set undolevels=1000
    set undoreload=10000
endif

let g:loaded_netrw       = 1
let g:loaded_netrwPlugin = 1

" let mapleader = "-"
let mapleader = "\<Space>"

let g:NumberToggleTrigger="<F3>"

" Escape insert mode quickly by typing 'kj'
:imap kj <Esc>

" Enable the list of buffers
let g:airline#extensions#tabline#enabled = 1

" Show just the filename
let g:airline#extensions#tabline#fnamemod = ':t'

" This allows buffers to be hidden if you've modified a buffer.
" This is almost a must if you wish to use buffers in this way.
set hidden

" To open a new empty buffer
" This replaces :tabnew which I used to bind to this mapping
nmap <leader>T :enew<CR>

" Move to the next buffer
nmap <leader>l :bnext<CR>

" Move to the previous buffer
nmap <leader>h :bprevious<CR>

" Close the current buffer and move to the previous one
" This replicates the idea of closing a tab
nmap <leader>bq :bp <BAR> bd #<CR>

" Show all open buffers and their status
nmap <leader>bl :ls<CR>

" Close all other splits except the focused one
nmap ä :only<CR>

" Save by pressing ö
nmap ö :w<CR>

" Open Silver Searcher with key Ä
nmap Ä :Ag<CR>

" Close a buffer quickly by pressing å in normal mode
nmap å :bd<CR>

" In Clojure, jump to symbol definition by pressing Ö on symbol
nmap Ö ]<C-d>

" In Clojure, run tests by pressing ° in normal mode
nmap ° :RunTests<CR>

" Switch buffers the same way as in Spacemacs - space b b.
nmap <leader>bb :b<Space>

let g:closetag_filenames = "*.xml,*.html,*.xhtml,*.phtml,*.php"
au FileType xml,html,phtml,php,xhtml,js let b:delimitMate_matchpairs = "(:),[:],{:}"

autocmd Filetype javascript setlocal ts=2 sw=2 sts=2 expandtab

" Run Flake8 (PEP8 conformance test) to each opened Python file
" autocmd BufWritePost *.py call Flake8()

" let g:python_host_prog = "~/.pyenv/versions/3.6.5/bin/python3"

let g:javascript_plugin_jsdoc = 1
set wildignore+=*/node_modules/*     " MacOSX/Linux

let g:ctrlp_custom_ignore = 'node_modules\|out\|target\|git'

let g:move_key_modifier = 'C'

" Settings for vim-smooth-scroll
" noremap <silent> <c-u> :call smooth_scroll#up(&scroll, 0, 2)<CR>
" noremap <silent> <c-d> :call smooth_scroll#down(&scroll, 0, 2)<CR>
" noremap <silent> <c-b> :call smooth_scroll#up(&scroll*2, 0, 4)<CR>
" noremap <silent> <c-f> :call smooth_scroll#down(&scroll*2, 0, 4)<CR>

" Compile Clojure namespace by pressing §
nnoremap § :Require<CR>

" Trim unwanted whitespaces by :call TrimWhiteSpace()
fun! TrimWhitespace()
  let l:save = winsaveview()
  %s/\s\+$//e
  call winrestview(l:save)
endfun

" A shorter way: :Siivous
command! Siivous call TrimWhitespace()

" Only the search pattern will pulse
let g:vim_search_pulse_mode = 'cursor_line'

" Format JSX also in files with .js suffix
let g:jsx_ext_required = 0

function SwitchBuffer()
  b#
endfunction

nmap <Tab> :call SwitchBuffer()<CR>
let g:fzf_action = {
      \ 'ctrl-s': 'split',
      \ 'ctrl-v': 'vsplit'
      \ }
nnoremap <c-p> :FZF<cr>

au BufNewFile,BufRead *.boot set filetype=clojure
