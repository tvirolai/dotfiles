" This allows buffers to be hidden if you've modified a buffer.
" This is almost a must if you wish to use buffers in this way.
" Required for operations modifying multiple buffers like rename.
set hidden
set nocompatible              " be iMproved, required
set nofoldenable

" Plugins are defined here using plug-vim
call plug#begin('~/.config/nvim/bundle')

Plug 'autozimu/LanguageClient-neovim', {
    \ 'branch': 'next',
    \ 'do': 'bash install.sh',
    \ }
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'mhartington/oceanic-next'
" Intellisense Engine
Plug 'neoclide/coc.nvim', { 'do': 'yarn install' }
Plug 'neoclide/coc.nvim', {'tag': '*', 'do': { -> coc#util#install()}}

" " Denite - Fuzzy finding, buffer management
" Plug 'Shougo/denite.nvim'

" " Snippet support
" Plug 'Shougo/neosnippet'
" Plug 'Shougo/neosnippet-snippets'
Plug 'guns/vim-clojure-highlight'
Plug 'SevereOverfl0w/vim-replant', { 'do': ':UpdateRemotePlugins' }
Plug 'luochen1990/rainbow'
Plug 'scrooloose/nerdtree'
Plug 'inside/vim-search-pulse'
" Plug 'pangloss/vim-javascript'
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
" Plug 'Valloric/YouCompleteMe', { 'do': './install.py --tern-completer --clang-completer' }
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
Plug 'thiagoalessio/rainbow_levels.vim'
Plug 'tpope/vim-fugitive'
Plug 'ElmCast/elm-vim'
Plug 'junegunn/gv.vim'
Plug 'junegunn/limelight.vim'
Plug 'junegunn/goyo.vim'
Plug 'junegunn/seoul256.vim'
Plug 'cocopon/iceberg.vim'
Plug 'ap/vim-css-color'
Plug 'dracula/vim'
Plug 'liquidz/vim-iced', {'for': 'clojure'}
Plug 'vim-airline/vim-airline-themes'
Plug 'tssm/fairyfloss.vim'
Plug 'mbbill/undotree'
call plug#end()


"""""""""""""""""""
" PLUGIN SETTINGS "
"""""""""""""""""""
let g:rainbow_active = 0 "0 if you want to enable it later via :RainbowToggle

" Configure vim-slime to be able to send data from the editor window
" to the REPL. Here Vim is assumed to be run on the left side of a vertically
" split tmux window, with the repl on the right.
let g:slime_target = "tmux"
let g:slime_default_config = {"socket_name": "default", "target_pane": "{right-of}"}

" Only the search pattern will pulse
let g:vim_search_pulse_mode = 'cursor_line'

let g:clojure_maxlines = 2000

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

nnoremap ¨ /
nnoremap ^ :

cnoremap w!! execute 'silent! write !sudo tee % >/dev/null' <bar> edit!

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

let g:closetag_filenames = "*.xml,*.html,*.xhtml,*.phtml,*.php,*.jsx,*.js"
let g:javascript_plugin_jsdoc = 1

au FileType xml,html,phtml,php,xhtml,js let b:delimitMate_matchpairs = "(:),[:],{:}"

"""""""""""""""""""
" APPEARANCE      "
"""""""""""""""""""
syntax enable
set termguicolors

let g:gruvbox_contrast_dark = 'soft'
let g:gruvbox_contrast_light = 'medium'

" set background=light

" colorscheme iceberg
" colorscheme dracula
" colorscheme OceanicNext
colorscheme gruvbox
" colorscheme fairyfloss
set background=dark

" let g:seoul256_background = 236
" colo seoul256

set cursorline " - Valitun rivin korostus
set colorcolumn=80

" " Enable the list of buffers
let g:airline#extensions#tabline#enabled                      = 1
let g:airline#extensions#tabline#buffer_min_count             = 1
let g:airline#extensions#tabline#tab_min_count                = 1
let g:airline#extensions#tabline#buffer_idx_mode              = 1
let g:airline#extensions#tabline#buffer_nr_show               = 1
let g:airline#extensions#tabline#show_buffers                 = 1
let g:airline#extensions#branch#enabled                       = 1
let g:airline#extensions#tagbar#enabled                       = 1
" let g:airline_powerline_fonts                                 = 1
let g:airline#extensions#whitespace#enabled       = 0
let g:airline#extensions#tabline#fnamemod         = ':t'
let g:airline_section_c                           = '%{fnamemodify(expand("%"), ":~:.")}'
let g:airline_section_x                           = '%{fnamemodify(getcwd(), ":t")}'
let g:airline_section_y                           = airline#section#create(['filetype'])

" Easier tab/buffer switching
nmap <leader>1 <Plug>AirlineSelectTab1
nmap <leader>2 <Plug>AirlineSelectTab2
nmap <leader>3 <Plug>AirlineSelectTab3
nmap <leader>4 <Plug>AirlineSelectTab4
nmap <leader>5 <Plug>AirlineSelectTab5
nmap <leader>6 <Plug>AirlineSelectTab6
nmap <leader>7 <Plug>AirlineSelectTab7
nmap <leader>8 <Plug>AirlineSelectTab8
nmap <leader>9 <Plug>AirlineSelectTab9

nmap ´ :Buffers<CR>

"""""""""""""""""""""""""""""
" GENERAL EDITOR SETTINGS   "
"""""""""""""""""""""""""""""
filetype plugin indent on    " required
" set maxmempattern=2000000
set timeoutlen=500
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

" enable autocomplete
" let g:deoplete#enable_at_startup = 1

let g:python_host_prog = $HOME."/.pyenv/versions/2.7.11/envs/neovim2/bin/python"
" let g:python_host_prog = "/usr/local/bin/python"
" let g:python3_host_prog = "/usr/local/bin/python3"
let g:python3_host_prog = $HOME."/.pyenv/versions/3.7.1/envs/neovim3/bin/python3"

" Format JSX also in files with .js suffix
let g:jsx_ext_required = 0

" Disable netrw for NerdTree
let g:loaded_netrw       = 1
let g:loaded_netrwPlugin = 1

au BufNewFile,BufRead *.boot set filetype=clojure
autocmd Filetype javascript setlocal ts=2 sw=2 sts=2 expandtab

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

" Rebind arrow keys. Up and down circle through to spots in the buffer where
" text has been last inserted, right goes to the line that was last edited.
" Left jumps to the last edit point and switches in insert mode.
nnoremap <Up> g;<CR>
nnoremap <Down> g,<CR>
nnoremap <Right> `.
nnoremap <Left> gi

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
" nmap <leader>bl :ls<CR>

" Close all other splits except the focused one
nmap ä :only<CR>

" Open Silver Searcher with key Ä
nmap Ä :Ag<CR>
"
" Save by pressing ö
nmap ö :w<CR>

" Close a buffer quickly by pressing å in normal mode (without closing open
" windows!)
nmap Å :bp<bar>sp<bar>bn<bar>bd<CR>
" This version closes splits:
nmap å :bd!<CR>

" Switch buffers by name/number the same way as in Spacemacs - space b b.
" nmap <leader>bb :b<Space>

" Clojure-specific bindings
" Compile Clojure namespace by pressing §
nmap § :Require<CR>
" Jump to definition by pressing Ö on symbol
nmap Ö ]<C-d>
" nmap ° cpp<CR>
" Run tests by pressing °
nmap ° :RunTests<CR>

function SwitchBuffer()
  b#
endfunction

" Toggle between two latest buffers by pressing TAB
nmap <Tab> :call SwitchBuffer()<CR>

" fugitive git bindings
nnoremap <leader>ga :Git add %:p<CR><CR>
nnoremap <leader>gs :Gstatus<CR>
nnoremap <leader>gc :Gcommit -v -q<CR>
nnoremap <leader>gt :Gcommit -v -q %:p<CR>
nnoremap <leader>gd :Gdiff<CR>
nnoremap <leader>ge :Gedit<CR>
nnoremap <leader>gr :Gread<CR>
nnoremap <leader>gw :Gwrite<CR><CR>
nnoremap <leader>gl :Git reflog<CR>
nnoremap <leader>gp :Ggrep<Space>
nnoremap <leader>gm :Gmove<Space>
nnoremap <leader>gb :Gblame<CR>
nnoremap <leader>gbr :Git branch<Space>
nnoremap <leader>go :Git checkout<Space>
" nnoremap <leader>gffs :Git flow feature start<CR>
nnoremap <leader>gps :Gpush<CR>
nnoremap <leader>gpl :Gpull<CR>

nnoremap <leader>gv :GV<CR>

""""""""""""
" VARIOUS  "
""""""""""""

set wildmode=list:full
set wildignore=*.swp,*.bak
set wildignore+=*.pyc,*.class,*.sln,*.Master,*.csproj,*.csproj.user,*.cache,*.dll,*.pdb,*.min.*
set wildignore+=*/.git/**/*,*/.hg/**/*,*/.svn/**/*
set wildignore+=tags
set wildignore+=*.tar.*

set wildignorecase
set wildignore+=*/node_modules/*     " MacOSX/Linux

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

function! s:goyo_enter()
  silent !tmux set status off
  silent !tmux list-panes -F '\#F' | grep -q Z || tmux resize-pane -Z
  set noshowmode
  set noshowcmd
  set scrolloff=999
  Limelight
  " ...
endfunction

function! s:goyo_leave()
  silent !tmux set status on
  silent !tmux list-panes -F '\#F' | grep -q Z && tmux resize-pane -Z
  set showmode
  set showcmd
  set scrolloff=5
  Limelight!
  " ...
endfunction

autocmd! User GoyoEnter nested call <SID>goyo_enter()
autocmd! User GoyoLeave nested call <SID>goyo_leave()
