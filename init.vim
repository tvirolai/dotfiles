set nocompatible              " be iMproved, required

" Plugins are defined here using plug-vim
call plug#begin('~/.config/nvim/bundle')

Plug 'autozimu/LanguageClient-neovim', {
    \ 'branch': 'next',
    \ 'do': 'bash install.sh',
    \ }
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'guns/vim-clojure-highlight'
Plug 'SevereOverfl0w/vim-replant', { 'do': ':UpdateRemotePlugins' }
Plug 'luochen1990/rainbow'
Plug 'scrooloose/nerdtree'
Plug 'inside/vim-search-pulse'
Plug 'pangloss/vim-javascript'
Plug 'alvan/vim-closetag'
Plug 'tpope/vim-commentary'
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
Plug 'reasonml-editor/vim-reason-plus'
Plug 'wellle/targets.vim'
Plug 'rust-lang/rust.vim'
Plug 'jpalardy/vim-slime'
call plug#end()


"""""""""""""""""""
" PLUGIN SETTINGS "
"""""""""""""""""""
let g:rainbow_active = 1 "0 if you want to enable it later via :RainbowToggle

" Configure vim-slime to be able to send data from the editor window
" to the REPL. Here Vim is assumed to be run on the left side of a vertically
" split tmux window, with the repl on the right.
let g:slime_target = "tmux"
let g:slime_default_config = {"socket_name": "default", "target_pane": "{right-of}"}

" Only the search pattern will pulse
let g:vim_search_pulse_mode = 'cursor_line'

" Format JSX also in files with .js suffix
let g:jsx_ext_required = 0

map <C-n> :NERDTreeToggle<CR>
let NERDTreeShowHidden=1

" Close vim if NerdTree is the last open buffer
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

let g:LanguageClient_serverCommands = {
    \ 'rust': ['~/.cargo/bin/rustup', 'run', 'stable', 'rls'],
    \ 'javascript': ['/usr/local/bin/javascript-typescript-stdio'],
    \ 'javascript.jsx': ['tcp://127.0.0.1:2089'],
    \ 'python': ['/usr/local/bin/pyls'],
    \ 'reason': ['/usr/local/bin/reason-language-server.exe']
    \ }

" enable autocomplete
let g:deoplete#enable_at_startup = 1

nnoremap <F5> :call LanguageClient_contextMenu()<CR>
" Or map each action separately
nnoremap <silent> gj :call LanguageClient#textDocument_hover()<CR>
nnoremap <silent> gd :call LanguageClient#textDocument_definition()<CR>
nnoremap <silent> <F2> :call LanguageClient#textDocument_rename()<CR>

" Get the results of previous fzf search using Ctrl-N / Ctrl-P
let g:fzf_history_dir = '~/.local/share/fzf-history'
let g:fzf_action = {
      \ 'ctrl-s': 'split',
      \ 'ctrl-v': 'vsplit'
      \ }
nnoremap <c-p> :FZF<cr>


"""""""""""""""""""
" APPEARANCE      "
"""""""""""""""""""
syntax enable
set termguicolors
colorscheme gruvbox
let g:gruvbox_contrast_dark = 'soft'
set background=dark
set cursorline " - Valitun rivin korostus
set colorcolumn=80

" Enable the list of buffers
let g:airline#extensions#tabline#enabled = 1

" Show buffer number on tab
let g:airline#extensions#tabline#buffer_nr_show = 1

" Show just the filename
let g:airline#extensions#tabline#fnamemod = ':t'

" This allows buffers to be hidden if you've modified a buffer.
" This is almost a must if you wish to use buffers in this way.
" Required for operations modifying multiple buffers like rename.
set hidden


"""""""""""""""""""""""""""""
" GENERAL EDITOR SETTINGS   "
"""""""""""""""""""""""""""""
filetype plugin indent on    " required
set regexpengine=1
set lazyredraw
set showcmd
set encoding=utf-8
set fileencoding=utf-8
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
set ai
set nu
set pastetoggle=<F2>
set mouse=
set backup
set swapfile
"" Text Wrapping
"set textwidth=79
set wrap
set nolist  " list disables linebreak

" Disable netrw for NerdTree
let g:loaded_netrw       = 1
let g:loaded_netrwPlugin = 1

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


"""""""""""""""""""""""
" CUSTOM KEYBINDINGS  "
"""""""""""""""""""""""

let mapleader = "\<Space>"

let g:NumberToggleTrigger="<F3>"

let g:move_key_modifier = 'C'

" Escape insert mode quickly by typing 'kj'
inoremap kj <Esc>

" Also remap Ctrl-C to work similarly to Esc
inoremap <C-c> <Esc>

" To open a new empty buffer
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

function SwitchBuffer()
  b#
endfunction

" Toggle between two latest buffers by pressing TAB
nmap <Tab> :call SwitchBuffer()<CR>


""""""""""""
" VARIOUS  "
""""""""""""

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

set maxmempattern=2000000

let g:closetag_filenames = "*.xml,*.html,*.xhtml,*.phtml,*.php,*.jsx,*.js"
au FileType xml,html,phtml,php,xhtml,js let b:delimitMate_matchpairs = "(:),[:],{:}"

autocmd Filetype javascript setlocal ts=2 sw=2 sts=2 expandtab

let g:javascript_plugin_jsdoc = 1
set wildignore+=*/node_modules/*     " MacOSX/Linux

let g:ctrlp_custom_ignore = 'node_modules\|out\|target\|git'

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

au BufNewFile,BufRead *.boot set filetype=clojure
