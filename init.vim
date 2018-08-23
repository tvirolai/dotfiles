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
Plug 'tpope/vim-classpath'
Plug 'tpope/vim-salve'
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
Plug 'elixir-editors/vim-elixir'
Plug 'jacoborus/tender.vim'
Plug 'kovisoft/slimv', { 'for': 'lisp' }
Plug 'junegunn/seoul256.vim'
Plug 'raphamorim/lucario'
Plug 'NLKNguyen/papercolor-theme'
call plug#end()

syntax enable
set synmaxcol=300
syntax sync minlines=256
set regexpengine=1

set termguicolors

set showcmd

set lazyredraw

" let ayucolor="mirage"

" A selection of nice color schemes to alternate between

" colorscheme dracula
" colorscheme monokai
" colorscheme onedark
colorscheme gruvbox
" colorscheme lucario
" colorscheme ayu
" colorscheme apprentice
" colorscheme tender
" let g:seoul256_background = 238
" colorscheme seoul256

" set t_Co=256
" set background=dark
" colorscheme PaperColor

let g:gruvbox_contrast_dark = 'soft'
filetype plugin indent on    " required

map <C-n> :NERDTreeToggle<CR>
let NERDTreeShowHidden=1

" Get the results of previous fzf search using Ctrl-N / Ctrl-P
let g:fzf_history_dir = '~/.local/share/fzf-history'

let g:rainbow_active = 1 "0 if you want to enable it later via :RainbowToggle
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

let mapleader = "\<Space>"

let g:NumberToggleTrigger="<F3>"

" Escape insert mode quickly by typing 'kj'
inoremap kj <Esc>

" Also remap Ctrl-C to work similarly to Esc
inoremap <C-c> <Esc>

" Enable the list of buffers
let g:airline#extensions#tabline#enabled = 1

" Show buffer number on tab
let g:airline#extensions#tabline#buffer_nr_show = 1

" Show just the filename
let g:airline#extensions#tabline#fnamemod = ':t'

" Trying some funky buffer stuff from https://stackoverflow.com/questions/16082991/vim-switching-between-files-rapidly-using-vanilla-vim-no-plugins

set path=.,**
nnoremap <leader>f :find *
nnoremap <leader>s :sfind *
nnoremap <leader>v :vert sfind *
nnoremap <leader>t :tabfind *

nnoremap <leader>F :find <C-R>=expand('%:h').'/*'<CR>
nnoremap <leader>S :sfind <C-R>=expand('%:h').'/*'<CR>
nnoremap <leader>V :vert sfind <C-R>=expand('%:h').'/*'<CR>
" nnoremap <leader>T :tabfind <C-R>=expand('%:h').'/*'<CR>

set wildcharm=<C-z>
nnoremap <leader>b :buffer <C-z><S-Tab>
nnoremap <leader>B :sbuffer <C-z><S-Tab>

nnoremap <PageUp>   :bprevious<CR>
nnoremap <PageDown> :bnext<CR>

nnoremap <leader>j :tjump /

set wildmode=list:full

set wildignore=*.swp,*.bak
set wildignore+=*.pyc,*.class,*.sln,*.Master,*.csproj,*.csproj.user,*.cache,*.dll,*.pdb,*.min.*
set wildignore+=*/.git/**/*,*/.hg/**/*,*/.svn/**/*
set wildignore+=tags
set wildignore+=*.tar.*

set wildignorecase
"""""""

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

" Open Silver Searcher with key Ä
nmap Ä :Ag<CR>
"
" Save by pressing ö
nmap ö :w<CR>

" Close a buffer quickly by pressing å in normal mode (without closing open
" windows!)
nmap å :bp<bar>sp<bar>bn<bar>bd<CR>
" This version closes splits:
" nmap å :bd<CR>

" Switch buffers by name/number the same way as in Spacemacs - space b b.
nmap <leader>bb :b<Space>

" Clojure-specific bindings

" Compile Clojure namespace by pressing §
nmap § :Require<CR>
" run tests with Å
nmap Å :RunTests<CR>
" Jump to definition by pressing Ö on symbol
nmap Ö ]<C-d>
" Evaluate form under cursor by pressing °
nmap ° cpp<CR>

let g:closetag_filenames = "*.xml,*.html,*.xhtml,*.phtml,*.php,*.jsx,*.js"
au FileType xml,html,phtml,php,xhtml,js let b:delimitMate_matchpairs = "(:),[:],{:}"

autocmd Filetype javascript setlocal ts=2 sw=2 sts=2 expandtab

let g:javascript_plugin_jsdoc = 1
set wildignore+=*/node_modules/*     " MacOSX/Linux

let g:ctrlp_custom_ignore = 'node_modules\|out\|target\|git'

let g:move_key_modifier = 'C'

" Settings for vim-smooth-scroll
" noremap <silent> <c-u> :call smooth_scroll#up(&scroll, 0, 2)<CR>
" noremap <silent> <c-d> :call smooth_scroll#down(&scroll, 0, 2)<CR>
" noremap <silent> <c-b> :call smooth_scroll#up(&scroll*2, 0, 4)<CR>
" noremap <silent> <c-f> :call smooth_scroll#down(&scroll*2, 0, 4)<CR>

" Trim unwanted whitespaces by :call TrimWhiteSpace()
fun! TrimWhitespace()
  let l:save = winsaveview()
  %s/\s\+$//e
  call winrestview(l:save)
endfun

" Fix scandinavian chars
fun! Scandics()
  let l:save = winsaveview()
  %s/å/\\u00e5/g
  %s/Å/\\u00c5/g
  %s/ä/\\u00e4/g
  %s/Ä/\\u00c4/g
  %s/ö/\\u00f6/g
  %s/Ö/\\u00d6/g
  call winrestview(l:save)
endfun

command! Scandics call Scandics()

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
