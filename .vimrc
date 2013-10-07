" Anton Pirogov's .vimrc
" vim: set foldmethod=marker:
" ======================

"let mapleader = ","            " Set leader key for keybindings (default: \)

" Vundle bundles {{{
filetype off                   " required!
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" let Vundle manage Vundle
" required! 
Bundle 'gmarik/vundle'

" Drop-In improvements (zeroconf)
Bundle 'matchit.zip'
Bundle 'Processing'
Bundle 'tpope/vim-speeddating'
Bundle 'tpope/vim-repeat'
Bundle 'tpope/vim-unimpaired'
Bundle 'sudar/vim-arduino-syntax'
Bundle 'hail2u/vim-css3-syntax'
Bundle 'othree/html5-syntax.vim'
Bundle 'kchmck/vim-coffee-script'

" Coding/IDE features
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-endwise'
Bundle 'tpope/vim-commentary'
Bundle 'scrooloose/nerdtree'
Bundle 'majutsushi/tagbar'
Bundle 'Raimondi/delimitMate'
Bundle 'ervandew/supertab'

" Language specific
" Bundle 'c.vim'
Bundle 'a.vim'
Bundle 'mattn/emmet-vim'
Bundle 'vim-scripts/VimClojure'
Bundle 'gberenfield/cljfold.vim' 

" Snipmate stuff
Bundle 'MarcWeber/vim-addon-mw-utils'
Bundle 'tomtom/tlib_vim'
Bundle 'garbas/vim-snipmate'
Bundle 'honza/vim-snippets'

" General vim usability
Bundle 'sjl/gundo.vim'
Bundle 'kien/ctrlp.vim'
Bundle 'corntrace/bufexplorer'
Bundle 'kshenoy/vim-signature'
Bundle 'Lokaltog/vim-easymotion'
Bundle 'tpope/vim-surround'
Bundle 'tpope/vim-abolish'
Bundle 'maxbrunsfeld/vim-yankstack'
Bundle 'christoomey/vim-tmux-navigator'
Bundle 'epeli/slimux'
Bundle 'DrawIt'

" Colors
Bundle 'altercation/vim-colors-solarized'

filetype plugin indent on " required!
" }}}

" Plugin config and keybindings {{{

" vimFugitive: add current git branch to statusline
set statusline+=%{fugitive#statusline()}

" vimCommentary - gc<Motion> -> toggle comment, gcu -> uncomment commented

" Toggle NERDTree
map <F10> :NERDTreeToggle<cr>

" Toggle Tag List
map <F11> :TagbarToggle<cr>

" delimitMate: disable in regular text
au FileType mail,text let b:delimitMate_autoclose = 0

" VimClojure
let vimclojure#HighlightBuiltins=1      " Highlight Clojure's builtins
let vimclojure#ParenRainbow=1           " Rainbow parentheses'!

" Snipmate
let g:snips_author = "Anton Pirogov"

" GUndo
nnoremap <F5> :GundoToggle<CR>

" CtrlP - C-j/C-k to next/prev completion, C-r regex mode, C-f/b cycle modes, C-t/v/s open in new tab/split
" BufExplorer - opens with <leader>be
" vimSignature - Toggle mark with mN, `N -> goto mark N, m<space> remove all marks
" EasyMotion - Toggle easy motion with <leader><leader>w or f or t
" vimSurround - cs"' changes "a" to 'a', ds" changes "a" to a, ysiw a on a -> 'a'

" yankStack: other keybindings (conflicting ctrlp)
let g:yankstack_map_keys = 0
nmap <leader>p <Plug>yankstack_substitute_older_paste
nmap <leader>P <Plug>yankstack_substitute_newer_paste

" Vim-tmux-navigator - C-hjkl to move between splits and also tmux panes

" Slimux
map <leader>t :SlimuxREPLSendLine<CR>
vmap <leader>t :SlimuxREPLSendSelection<CR>
map <leader>a :SlimuxShellLast<CR>
map <leader>k :SlimuxSendKeysLast<CR>

" DrawIt - <leader>di -> start, <leader>ds -> stop

" }}}

" Misc. Keybindings {{{

"Highlight tabs
syntax match Tab /\t/
hi Tab guifg=grey ctermbg=grey

" Paste from clipboard
nmap <C-v><C-v> "+p

" Tab keybindings
map <C-t> :tabnew<CR>
nnoremap <A-Left> :tabprevious<CR>
nnoremap <A-Right> :tabnext<CR>
nnoremap <silent> <A-Up> :execute 'silent! tabmove ' . (tabpagenr()-2)<CR>
nnoremap <silent> <A-Down> :execute 'silent! tabmove ' . tabpagenr()<CR>

" Resize splits
nnoremap <C-Left> <C-w><
nnoremap <C-Right> <C-w>>
nnoremap <C-Up> <C-w>-
nnoremap <C-Down> <C-w>+

" Toggle tabs
let notabs = 1
nnoremap <silent> <F9> :let notabs=!notabs<Bar>:if notabs<Bar>:tabo<Bar>:else<Bar>:tab ball<Bar>:tabn<Bar>:endif<CR>

"Sort a file
map <leader>s :%!sort<cr>
"Wrap on/off
map <leader>w :set wrap!<cr>
"apply ruby one liner to a range ($_ = current line)
map <leader>r :rubydo $_ = 
" Disable search highlighting
nmap <silent> <leader>/ :nohlsearch<CR>

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

" disable arrow keys
" noremap  <Up> ""
" noremap! <Up> <Esc>
" noremap  <Down> ""
" noremap! <Down> <Esc>
" noremap  <Left> ""
" noremap! <Left> <Esc>
" noremap  <Right> ""
" noremap! <Right> <Esc>

" Arrow keys for indentation
nmap <Left> <<
nmap <Right> >>
vmap <Left> <gv
vmap <Right> >gv
" Arrow keys for line swapping (unimpaired)
nmap <Up> [e
nmap <Down> ]e
vmap <Up> [egv
vmap <Down> ]egv

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
"Tip: Convert file to hex editor output: :%!xxd

" }}}

" General {{{
set nocompatible               " prevent vi emulation
set encoding=utf-8             " set default encoding
set fileformats=unix,dos,mac   " set ordered list of supported file fmts
set shell=/bin/bash            " just to be sure
set directory=~/.vim/tmp       " Dir for vim temp files

" Config files & registers (e.g. undo)
set nobackup                   " make no backups of files
"set backupdir=~                " not required
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
set backspace=indent,eol,start " improve backspace in insert mode
set esckeys                    " cursor keys in insert mode
set timeoutlen=500             " terminal esc timeout
set ttimeoutlen=50             " terminal esc timeout

" Search behaviour & completition & matching
set hlsearch                   " highlight search matches
set incsearch                  " let vim search while typing the search
set ignorecase                 " case insensitive matching
set smartcase                  " override ignorecase if regex contains uppercase chars
set magic                      " use 'magic' patterns (extended regular expressions)
set infercase                  " adjust cast of auto completed words
set showmatch                  " show matching parantheses while typing
set matchtime=5                " how many 1/10 seconds to blink
set matchpairs+=<:>            " Have % bounce between angled brackets and others

" Indentation & tabs
"set autoindent                 " assume indentation for next line by lvl of current line
"set copyindent                 " copy found indentation style in the file
"set preserveindent             " only add as much chars as required 
set smartindent                " enable intelligent guessing of indentation
set tabstop=2                  " set tab size to 4
set shiftwidth=2               " set shiftwidth to 4
set expandtab                  " replace tabs with spaces

" Scroll behaviour
set scrolloff=5                " keep a context when scrolling
set sidescrolloff=5            " same thing horizontally
set ttyfast                    " we have a fast terminal connection
"set ttyscroll=0                " turn off scrolling (faster)

" Wrapping
set nowrap                     " disable wrapping (cause its bad for coding)
set showbreak=+                " show a + if a line is longer than the screen
"set textwidth=0                " after how many chars a break is made
"set linebreak                  " wrap on word boundaries (not in word middle)
"set cpoptions+=n               " show break char between line numbers

" Folding
set foldmethod=syntax          " fold mode - by syntax. control: zo, zc
set foldnestmax=5              " max. nested folds
set foldenable                 " enable autofolding
set foldlevel=5                " Level from which autofold
set foldcolumn=1               " columns for fold markers
"set foldmarker={,}             " Fold C style code (only use if you use high f. level)
"set foldopen-=search	          " don't unfold while searching
"set foldopen-=undo             " don't unfold when undo sth.

" Improvement & tweaks
set tabpagemax=20              " maximum number of tabs to create
"set showtabline=2             " always show tab bar
set nojoinspaces               " don't insert 2 spaces after .?! if 2 lines get j.
set undolevels=1000            " number of undo levels
set confirm                    " confirm actions
set gdefault                   " set 'g' flag by default with :s/foo/bar/
set autowrite                  " automatically save before :next, :make etc.
"set list                      " shows whether tabs or spaces are used
"set listchars=tab:>-,trail:·,eol:$,  " which chars to show for tabs etc.
set listchars+=extends:\\,precedes:\\	" show not wrapped long lines
set nostartofline              " don't jump to first char with page commands
set nohidden                   " Don't make me miss changes in hidden buffers
set report=0                   " if lines got changed - notify
"set virtualedit=all            " let cursor move around beyond text
"set ofu=syntaxcomplete#Complete      " Insert mode tab completion (builtin)

" Status line & appearance
set background=dark            " terminal background
colorscheme wir_black          " my colorscheme for console mode
"set title                     " shows title text in console title bar
set statusline=%<%F%h%m%r%h%w%y\ %{&ff}\ %{strftime(\"%d/%m/%Y-%H:%M\")}%=\ l:%l\/%L\ col:%c%V\ pos:%o\ ascii:%b\ %P\ 
set t_Co=256                   " 256 Color mode
set cmdheight=1                " command bar is 2 high
set showcmd                    " show (partial) command in status line
set showmode                   " show current mode
set ruler                      " show cursor position in status line
set modeline                   " enable modeline
set laststatus=2               " when to show a statusline
set number                     " show line number
set numberwidth=4              " hopefully I never have more than 9999 lines per file
set lazyredraw                 " no window redraw while running macros (faster)
set wildmenu                   " turn on wild menu (completition feature)
set wildmode=longest:list      " turn on long format
set wildignore=*.dll,*.o,*.obj,*.exe,*.swp,*.jpg       " ignore some formats
if has("gui_running")
  set background=dark
  colorscheme solarized        " GUI scheme
	set cursorline               " highlight current line
	set guifont=Monaco           " Font for GVim
	set guioptions-=T            " Remove toolbar in GUI mode
	set guioptions-=e            " Remove gui tabs (console tabs)
	"set guioptions=acg           " Remove all menus, toolbars and stuff
	"set nomousehide              " don't hide mouse while typing
  "set lines=25                 " standard console size
	"set columns=80               " standard console size
endif

" Anti annoyance
set vb t_vb=                   " prevent annoying beeps, flash screen instead
set novisualbell               " don't blink
set noerrorbells               " no beeps
set shortmess=aoOstI           " shortens messages to avoid "press a key" prompt
set clipboard+=unnamed         " yank & copy to X clipboard

" Enable syntax highlighting
if has("syntax")
	syntax on
  set popt+=syntax:y           " Syntax when printing
endif
" }}}

" Language specific stuff {{{
if has("autocmd")
  " Enabled file type detection and file-type specific plugins.
  filetype plugin indent on " filetype plugin and indent
  autocmd FileType ruby setlocal expandtab shiftwidth=2 softtabstop=2	"ruby specific

  " Filetype-specific execute commands
  " F7 = Load into REPL (if any)
  " F8 = Compile this single file and run executable / Run with interpreter

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
  au BufRead,BufNewFile *.tex     map <F8>  :w<cr>:cd %:p:h<cr>:!pdflatex *.tex && evince *.pdf<cr><cr>

endif
" }}}

" Misc improvement scripts {{{

" when we reload, tell vim to restore the cursor to the saved position
au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif

" Remove trailing whitespace from code files on save
fun! <SID>StripTrailingWhitespaces()
    let l = line(".")
    let c = col(".")
    %s/\s\+$//e
    call cursor(l, c)
endfun
autocmd FileType c,cpp,java,php,ruby,python autocmd BufWritePre <buffer> :call <SID>StripTrailingWhitespaces()
" }}}
