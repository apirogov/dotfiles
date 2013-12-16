" Anton Pirogov's .vimrc
" vim: set foldmethod=marker foldlevel=0:
" TODO: Replace stuff with Neobundle, vimfiler, vimshell, unite.vim and compatible stuff?
" ======================
let mapleader = ","            " Set leader key for keybindings (default: \)

" General defaults {{{
" -------------------------------------------------------------
set nocompatible               " prevent vi emulation
set backspace=indent,eol,start " Sane backspace behaviour
set encoding=utf-8             " set default encoding
set fileformats=unix,dos,mac   " set ordered list of supported file fmts
set shell=/bin/bash            " Shell used by :sh
set directory=~/.vim/tmp       " Dir for vim temp files
set undodir=~/.vim/tmp
set backupdir=~/.vim/tmp
set undofile
set nobackup                   " make no backups of files
set history=100                " number of lines of command line history
" Tell vim to remember certain things when we exit
"  '10  :  marks will be remembered for up to 10 previously edited files
"  "100 :  will save up to 100 lines for each register
"  :20  :  up to 20 lines of command-line history will be remembered
"  %    :  saves and restores the buffer list
"  n... :  where to save the viminfo files
set viminfo='10,\"100,:20,%,n~/.viminfo

" Mouse & keys
set mouse=a                    " mouse in all modes
set esckeys                    " cursor keys in insert mode

" Search behaviour & Command completion & Parens matching
set incsearch                  " Incremental search
set hlsearch                   " highlight search matches
set ignorecase                 " case insensitive matching
set smartcase                  " override ignorecase if regex contains uppercase chars
set magic                      " use 'magic' patterns (extended regular expressions)
set infercase                  " adjust case of auto completed words
set wildmenu                   " turn on wild menu (completition feature)
set wildmode=list:longest,full " turn on long format
set wildignore=*.dll,*.o,*.obj,*.exe,*.swp,*.jpg       " ignore some formats
set showmatch                  " Show matching parens
set matchpairs+=<:>            " Have % bounce between angled brackets and others

" Indentation & tabs
set smarttab                   " Tab in front of line indents
set smartindent                " enable intelligent guessing of indentation
set tabstop=2                  " A tab is n spaces
set expandtab                  " replace tabs with spaces
set softtabstop=2              " Insert n spaces when tab is pressed
set shiftwidth=2               " An indent is n spaces
set shiftround                 " Round indent to multiple of shiftwidth

" Wrapping
set nowrap                     " disable wrapping (cause its bad for coding)
set showbreak=+                " show a + if a line is longer than the screen
set linebreak                  " wrap on word boundaries (not in word middle)
set textwidth=90               " after how many chars a break is made
" set colorcolumn=+1             " Highlight that column (more annoying)
"set cpoptions+=n               " show break char between line numbers

" Folding
set foldmethod=syntax          " fold mode - by syntax. control: zo, zc
set foldnestmax=5              " max. nested folds
set foldenable                 " enable autofolding
set foldlevel=5                " Level from which autofold
set foldcolumn=1               " columns for fold markers
"set foldopen-=search	          " don't unfold while searching
"set foldopen-=undo             " don't unfold when undo sth.

" Improvement & tweaks
set ttimeout
set ttimeoutlen=50
set display+=lastline          " Keep last line while scrolling
set ttyfast                    " we have a fast terminal connection -> smoothness
set lazyredraw                 " no window redraw while running macros (faster)
set autoread                   " Automatically read file on external changes
set undolevels=1000            " number of undo levels
set tabpagemax=20              " maximum number of tabs to create
set nostartofline              " don't jump to first char with page commands
set nojoinspaces               " don't insert 2 spaces after .?! if 2 lines get j.
set nrformats-=octal           " C-A/C-X work as dec even for nums starting with leading 0
set confirm                    " confirm actions
set gdefault                   " set 'g' flag by default with :s/foo/bar/
set autowrite                  " automatically save before :next, :make etc.
set report=0                   " if lines got changed - always notify
set listchars=tab:>\ ,trail:-,extends:>,precedes:<,nbsp:+ " Show these non-printables

if has("syntax")
	syntax on
  set popt+=syntax:y           " Syntax highlighting also when printing
endif

" Status line & appearance
set background=light           " terminal background
set laststatus=2               " Always show statusline
set ruler                      " Show current position
set showcmd                    " Show partial command in statusline
set cmdheight=1                " command bar is 1 high
set showmode                   " show current mode
set number                     " show line number
set t_Co=256                   " Number of colors in terminal

if has("gui_running")
	set cursorline               " highlight current line
	set guifont=Inconsolata\ 10  " Font for GVim
	set guioptions-=Te            " Remove toolbar and GUI tabs (use console tabs)
  "set lines=25                 " standard console size
	"set columns=80               " standard console size
endif

" Anti Annoyance
set vb t_vb=                   " prevent annoying beeps, flash screen instead
set novisualbell               " don't blink
set noerrorbells               " no beeps
set shortmess=aoOstI           " shortens messages to avoid "press a key" prompt
" set clipboard+=unnamedplus
set clipboard=unnamedplus      " yank & copy to X clipboard (+ register, *=selection reg.)
" }}}

" Vundle bundles {{{
" ==============
" Vundle init {{{
" ------------------
" Setting up Vundle - the vim plugin bundler
let iCanHazVundle=1
let vundle_readme=expand('~/.vim/bundle/vundle/README.md')
if !filereadable(vundle_readme)
  echo "Installing Vundle.."
  echo ""
  silent !mkdir -p ~/.vim/bundle
  silent !git clone https://github.com/gmarik/vundle ~/.vim/bundle/vundle
  let iCanHazVundle=0
endif

filetype off                   " required!
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" let Vundle manage Vundle, required!
Bundle 'gmarik/vundle'
" }}}

" Looks {{{
" =====
" Colors
Bundle 'altercation/vim-colors-solarized'
Bundle 'w0ng/vim-hybrid'
Bundle 'tomasr/molokai'
Bundle 'zaiste/Atom'
Bundle 'sjl/badwolf'
Bundle 'vim-scripts/tir_black'
Bundle 'jnurmine/Zenburn'
Bundle 'vim-scripts/Wombat'
Bundle 'vim-scripts/summerfruit256.vim'
Bundle 'rainux/vim-desert-warm-256'
Bundle 'vim-scripts/jellybeans.vim'
Bundle 'vim-scripts/pyte'
Bundle '29decibel/codeschool-vim-theme'

" Colorscheme switcher
Bundle 'xolox/vim-colorscheme-switcher'
" Approximate GVim themes in terminal
Bundle 'godlygeek/csapprox'
" }}}

" General improvements {{{
"===========================================
" Open and create files and dirs
" ------------------------------
" Open file at some line :e file:line
Bundle 'bogado/file-line'
" File browser
Bundle 'scrooloose/nerdtree'

" Movement
" --------
" Improved %
Bundle 'matchit.zip'
" Smooth scrolling with C-D,C-U,C-B,C-F
Bundle 'terryma/vim-smooth-scroll'
" f/t on steroids (,,f/,,t/,,w/,,b)
Bundle 'Lokaltog/vim-easymotion'
" Toggle mark with mN, `N -> goto mark N, m<space> remove all marks
Bundle 'kshenoy/vim-signature'
" f/t with 2 chars (s/S<char><char>)
" Bundle 'justinmk/vim-sneak'

" Search and replace
" ------------------
" Search with number of matches
Bundle 'vim-scripts/IndexedSearch'
" Tab complete in Search
Bundle 'vim-scripts/SearchComplete'
" Start */# search from visual block
Bundle 'bronson/vim-visual-star-search'

" Edit
" ----
" Emacs-like yanking (,p/,P), see stack with :Yanks
Bundle 'maxbrunsfeld/vim-yankstack'
" Parallel editing with multiple cursors with C-n
Bundle 'terryma/vim-multiple-cursors'
" Alignment on some pattern with :Tabularize /char
Bundle 'godlygeek/tabular'
" Manage surroundings with [dcy]s<motion><surrounding>
Bundle 'tpope/vim-surround'
" Easier change inside surroundings with ,ci/,cas
Bundle 'briandoll/change-inside-surroundings.vim'
" C-a/C-x for dates
Bundle 'tpope/vim-speeddating'
" repeat with . for plugins
Bundle 'tpope/vim-repeat'
" Exchange places with cx<movement> cx<movement>
Bundle 'tommcdo/vim-exchange'

" Convenience, UI
" ---------------
" Status line on steroids
Bundle 'bling/vim-airline'
" Undo tree visualization
Bundle 'sjl/gundo.vim'
" Emacs-like scratch buffer with :Scratch
Bundle 'vim-scripts/scratch.vim'
" Buffer overview with ,be
Bundle 'corntrace/bufexplorer'
" Change/kill buffer with H/L/X
Bundle 'ngn/vim-buffing-wheel'
" Open files, mru and buffers with fuzzy search with C-p
Bundle 'kien/ctrlp.vim'
" Generic plugin to search information from different sources (Ido-mode?)
Bundle 'Shougo/vimproc.vim'
Bundle 'Shougo/unite.vim'
" }}}

" Code editing/IDE/Project features (generic) {{{
" ===========================================
" Code editing
" ------------
" Code Commenting - gc<Motion> -> toggle comment, gcu -> uncomment commented
Bundle 'tpope/vim-commentary'
" Automatic block closing (if ... end, etc)
Bundle 'tpope/vim-endwise'
" Auto-Closing for symbols
Bundle 'Raimondi/delimitMate'
" Color parentheses and other surroundings
Bundle 'kien/rainbow_parentheses.vim'
" Tab completion
Bundle 'ervandew/supertab' 
" Split/Join lines more easily with gS/gJ depending on context
Bundle 'AndrewRadev/splitjoin.vim'
" Snipmate
Bundle 'MarcWeber/vim-addon-mw-utils'
Bundle 'tomtom/tlib_vim'
Bundle 'garbas/vim-snipmate'
Bundle 'honza/vim-snippets'

" Code browsing
" -------------
" Ag (the silver searcher) search (awesome code grep with :Ag)
Bundle 'rking/ag.vim'
" Code browser
Bundle 'majutsushi/tagbar'
" Automatic tag file updates for tagbar with :UpdateTags
" Bundle 'xolox/vim-easytags'

" Project and session management
" ------------------------------
" tmux/vim interop: C-hjkl to move between splits and also tmux panes
Bundle 'christoomey/vim-tmux-navigator'
" Allow project-local vim config (.lvimrc)
Bundle 'embear/vim-localvimrc'
" Better session management with :Save/OpenSession
Bundle 'xolox/vim-misc'
Bundle 'xolox/vim-session'

" Checking, evaluating and compiling
" ----------------------------------
" Async compiling
Bundle 'tpope/vim-dispatch'
" slime-like repl interaction
Bundle 'epeli/slimux'
" Generate view of FIXME/TODO/XXX with <leader>t
Bundle 'vim-scripts/TaskList.vim'
" Syntax checking for vim
Bundle 'scrooloose/syntastic'

" Version control
" ---------------
" Git integration
Bundle 'tpope/vim-fugitive'
" Gitk clone within vim (fugitive-extension)
Bundle 'gregsexton/gitv'
" See changed lines within file :GitGutterToggle
Bundle 'airblade/vim-gitgutter'

" Syntax support
" --------------
Bundle 'Processing'
Bundle 'sudar/vim-arduino-syntax'
Bundle 'hail2u/vim-css3-syntax'
Bundle 'othree/html5-syntax.vim'
Bundle 'joedicastro/vim-markdown'
Bundle 'kchmck/vim-coffee-script'
Bundle 'gberenfield/cljfold.vim'
Bundle 'Twinside/vim-syntax-haskell-cabal'

" Useful text objects and movement operators
" ------------------------------------------
" Bundle 'lucapette/vim-textobj-underscore'
" Bundle 'nelstrom/vim-textobj-rubyblock'
" Bundle 'kana/vim-textobj-user'
" Bundle 'kana/vim-textobj-function'
" Bundle 'kana/vim-textobj-entire'
" Bundle 'vim-scripts/camelcasemotion'

" Language specific plugins
" -------------------------
" Latex
Bundle 'LaTeX-Box-Team/LaTeX-Box'
" Haskell
Bundle 'lukerandall/haskellmode-vim'
" Clojure
Bundle 'guns/vim-clojure-static'
Bundle 'tpope/vim-fireplace'
Bundle 'vim-scripts/paredit.vim'
" C/C++
Bundle 'a.vim'
Bundle 'Rip-Rip/clang_complete'
" Bundle 'vim-scripts/gdbmgr'
" }}}

" Misc plugins {{{
" ====
" Useful misc
" -----------
" Show info about characters with ga
Bundle 'tpope/vim-characterize'
" Colorize in right color with :ColorHighlight
Bundle 'chrisbra/color_highlight'
" Draw diagrams: <leader>di -> start, <leader>ds -> stop
Bundle 'DrawIt'
" HTML Un/escape special characters with ,he/,hu
Bundle 'skwp/vim-html-escape'
" :Hammer to compile markup to html and open browser
Bundle 'matthias-guenther/hammer.vim'
" Universal Text Linking - Execute URLs, etc
Bundle 'vim-scripts/utl.vim'

" Organization
" ------------
" Vim outliner (.otl files)
" Bundle 'vimoutliner/vimoutliner'
" Navigate outlines (:Voom vimoutliner)
" Bundle 'vim-voom/VOoM'
" Basic plain text wiki
" Bundle 'vim-scripts/potwiki.vim'
" Vim notes
" Bundle 'xolox/vim-notes'

" Fun
" ---
" Nyan Cat!
Bundle 'koron/nyancat-vim'
" Tetris!
Bundle 'vim-scripts/TeTrIs.vim'
" }}}

" Vundle finalize {{{
" ===============
if iCanHazVundle == 0
  echo "Installing Bundles, please ignore key map error messages"
  echo ""
  :BundleInstall

endif

"Enables filetype-specific plugins and indentation after Vundle is done
filetype plugin indent on " required!
" }}}
" }}}

" Plugin settings {{{
let g:yankstack_map_keys = 0              " Use other yankstack bindings, no default

let g:session_autosave = 'no'             " Disable asking about saving sessions
let g:session_autoload = 'no'

" Colorscheme switcher and colorscheme settings
let g:colorscheme_switcher_define_mappings = 0
let g:colorscheme_switcher_exclude = ['blue','darkblue','default','delek','desert','elflord','evening','koehler','morning','murphy','pablo','peachpuff','ron','shine','slate','torte','zellner']
if !has("gui_running")
  let g:solarized_termcolors=256
  let g:rehash256 = 1 " For molokai
  let g:colorscheme_switcher_exclude += ['atom','codeschool','wombat','pyte']
endif

" Add surrounding #{ ... } (ruby string var. interpolation)
let g:surround_113 = "#{\r}" " v
let g:surround_35 = "#{\r}" " #
" ysiwc cmd -> \cmd{bla} for latex
let g:surround_{char2nr('c')} = "\\\1command\1{\r}" 

" Syntastic
let g:syntastic_enable_signs=1    "mark syntax errors with :signs
let g:syntastic_auto_jump=0       "automatically jump to the error when saving the file
let g:syntastic_auto_loc_list=1   "show the error list automatically
let g:syntastic_quiet_warnings=1  "don't care about warnings

" CtrlP - C-j/C-k to next/prev completion, C-r regex mode, C-f/b cycle modes, C-t/v/s open in new tab/split TODO: backspace-> ..

let g:snips_author = "Anton Pirogov"  " Snipmate

" vimFugitive: add current git branch to statusline
set statusline+=%{fugitive#statusline()}

" delimitMate: disable in regular text
au FileType mail,text let b:delimitMate_autoclose = 0

" Enable rainbow parens everywhere
au VimEnter * RainbowParenthesesToggle

" Outliner
" au FileType vo_base :Voom vimoutliner
au BufEnter *.otl setlocal tabstop=2
au BufEnter *.otl setlocal shiftwidth=2

" Haskell mode
let g:haddock_browser = "firefox"
" }}}

" Keybindings {{{
" F-Keys
" ------
" Colorschemes
map <silent><F12> :NextColorScheme<cr>
map <silent><S-F12> :PrevColorScheme<cr>

" Gundo
nnoremap <silent><F11> :GundoToggle<CR>
" Yankstack
nnoremap <silent><S-F11> :Yanks<cr>

" Toggle Tag List
map <F10> :TagbarToggle<cr>
" Toggle DirTree
map <F9> :NERDTreeToggle<cr>

" Chords (Ctrl,Alt)
" ---------------------
" Tab keybindings
map <C-t> :tabnew<CR>
nnoremap <A-Left> :tabprevious<CR>
nnoremap <A-Right> :tabnext<CR>
nnoremap <silent> <A-Up> :execute 'silent! tabmove ' . (tabpagenr()-2)<CR>
nnoremap <silent> <A-Down> :execute 'silent! tabmove ' . tabpagenr()<CR>
" Toggle tabs
" let notabs = 1
" nnoremap <silent> <F9> :let notabs=!notabs<Bar>:if notabs<Bar>:tabo<Bar>:else<Bar>:tab ball<Bar>:tabn<Bar>:endif<CR>

" Resize splits
nnoremap <C-Left> <C-w><
nnoremap <C-Right> <C-w>>
nnoremap <C-Up> <C-w>-
nnoremap <C-Down> <C-w>+

" Smooth scrolling
noremap <silent> <c-u> :call smooth_scroll#up(&scroll, 10, 2)<CR>
noremap <silent> <c-d> :call smooth_scroll#down(&scroll, 10, 2)<CR>
noremap <silent> <c-b> :call smooth_scroll#up(&scroll*2, 20, 4)<CR>
noremap <silent> <c-f> :call smooth_scroll#down(&scroll*2, 20, 4)<CR>

" Arrow keys
" ----------
nmap <Left> <<
nmap <Right> >>
vmap <Left> <gv
vmap <Right> >gv
nmap <Up> ddkkp
nmap <Down> ddp

" Leader keys
" ----------
" Slimux
map <leader>s :SlimuxREPLSendLine<CR>
vmap <leader>s :SlimuxREPLSendSelection<CR>
" map <leader>a :SlimuxShellLast<CR>
" map <leader>k :SlimuxSendKeysLast<CR>

" Yankstack cycling
nmap <leader>p <Plug>yankstack_substitute_older_paste
nmap <leader>P <Plug>yankstack_substitute_newer_paste
" Buf Select
map <leader>bb :CtrlPBuffer<cr>
" MRU select
map <leader>bm :CtrlPMRUFiles<cr>
"Wrap on/off
map <leader>w :set wrap!<cr>
"apply ruby one liner to a range ($_ = current line)
map <leader>r :rubydo $_ = 
" Disable highlight of last search
map <leader>/ :nohlsearch<cr>
"Take rest of line and put above (reversed split)
nmap <leader>j d$O<esc>p
"Add beautiful line out of minus signs of correct length in code comments
nmap <leader>l zzpwv$r-
" }}}

" Language specific settings and keybindings {{{
" ----------------------------------------------
" Include completion for filetypes with custom lists
au FileType * exec("setlocal dictionary+=".$HOME."/.vim/dict/".expand('<amatch>'))
set complete+=k

" au FileType ruby setlocal expandtab shiftwidth=2 softtabstop=2	"ruby specific

" Filetype-specific keybindings
" F7 = Load into REPL (if any) F8 = Compile this single file and run executable / Run with interpreter

" Save and compile without make file and run small C program (not useful on multifile project)
au BufRead,BufNewFile  *.c,*.h  map <F8>	:w<cr>:!cc % && ./a.out<cr>

" Save and compile with ant
au BufRead,BufNewFile  *.java   map <F8>	:w<cr>:!ant<cr>

" Save and run ruby interpreter
au BufRead,BufNewFile  *.rb     map <F8>  :w<cr>:!ruby %<cr>
" Load into IRB
au BufRead,BufNewFile  *.rb     map <F7>  :w<cr>:!irb -r ./%<cr>

" Compile a standalone haskell script
au BufRead,BufNewFile *.hs      map <F8>  :w<cr>:!rm -rf /tmp/*.o; ghc -fwarn-name-shadowing -hidir=/tmp -odir=/tmp -O -o a.out % && ./a.out<cr>
" Load into GHCI
au BufRead,BufNewFile *.hs      map <F7>  :w<cr>:!ghci %<cr>

" Compile a pdf from latex
" au BufRead,BufNewFile *.tex     map <F8>  :w<cr>:cd %:p:h<cr>:!lualatex *.tex<cr><cr>
au BufRead,BufNewFile *.tex     map <F8>  :w<cr>:Latexmk<cr>
" }}}

" Misc improvements and settings {{{
" ---------------------
colorscheme solarized          " default color scheme

"Write to file without previous write access (asks password)
cmap w!! %!sudo tee > /dev/null %
" Switch current directory to that of the file
cmap cwd lcd %:p:h

" Fix Modifier+Arrow combinations within tmux
if &term =~ '^screen'
    " tmux will send xterm-style keys when its xterm-keys option is on
    execute "set <xUp>=\e[1;*A"
    execute "set <xDown>=\e[1;*B"
    execute "set <xRight>=\e[1;*C"
    execute "set <xLeft>=\e[1;*D"
endif

"Highlight tabs
syntax match Tab /\t/
hi Tab guifg=grey ctermbg=grey

" when we reload, tell vim to restore the cursor to the saved position
au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif

" Remove trailing whitespace from code files on save
fun! <SID>StripTrailingWhitespaces()
    let l = line(".")
    let c = col(".")
    %s/\s\+$//e
    call cursor(l, c)
endfun
au FileType c,cpp,java,php,ruby,python,tex,haskell,clojure autocmd BufWritePre <buffer> :call <SID>StripTrailingWhitespaces()

" ---------- Memos ------------
"Tip: Makros
"Record:
"q<some key>
"<edit one line and move to the next>
"q
"Play:
"@<some key>
"@@ (play last macro)
"100@<some key> repeat macro 100 times
"Tip: Increment/Decrement number: C-A/C-X
"Tip: Revert file state to some time: :earlier <time, e.g. 15m>
"or :later <time> to reverse it back (all based on undo)
"Tip: Convert file to hex editor output: :%!xxd, revert with :!xxd -r
"Tip: Find non-ascii: /[^\x00-\xff]
"Tip: !cmd runs external commands, r! cmd -> paste output into buffer
"Tip: dis,das delete in/around sentence (modifiers a and i, object s)
"Tip: zz = center around cursor,zt=cursor top, ZZ = :wq, D = d$
"Tip: look g commands - eg. gv reselects last visual selection
"Tip: :%TOhtml -> render file to html
"Tip: Move cursor to old/new pos C-o/C-i
"Tip: <range>rubydo $_ = stuff <- changes lines with ruby code
" }}}

