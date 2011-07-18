" Anton Pirogov's .vimrc

" General
set nocompatible				" prevent vi emulation
set encoding=utf-8				" set default encoding
set fileformats=unix,dos,mac	" set ordered list of supported file fmts
set shell=/bin/bash				" just to be sure xD
set directory=~/.vim/temp 		" Dir for vim temp files

" Config files & registers (e.g. undo)
set nobackup					" make no backups of files
"set backupdir=~				" not required
set history=100				" number of lines of command line history
set viminfo='20,\"50		" r/w a .viminfo file, don't store >50 lines

" Mouse & keys
set mouse=a					" mouse in all modes
set backspace=indent,eol,start	" improve backspace in insert mode
set esckeys						" cursor keys in insert mode

" Search behaviour & completition & matching
set hlsearch	" highlight search matches
set incsearch	" let vim search while typing the search
set ignorecase	" case insensitive matching
set smartcase	" override ignorecase if regex contains uppercase chars
set magic		" use 'magic' patterns (extended regular expressions)
set infercase	" adjust cast of auto completed words
set showmatch	" show matching parantheses while typing
set matchtime=5	" how many 1/10 seconds to blink
set matchpairs+=<:>	" Have % bounce between angled brackets and others

" Indentation & tabs
set autoindent	" assume indentation for next line by lvl of current line
set smartindent	" enable intelligent guessing of indentation
"set copyindent	" copy found indentation style in the file
"set preserveindent	" only add as much chars as required 
set tabstop=2		" set tab size to 4
set shiftwidth=4	" set shiftwidth to 4
set expandtab		" replace tabs with spaces

" Scroll behaviour
set scrolloff=5		" keep a context when scrolling
set sidescrolloff=5	" same thing horizontally
"set ttyscroll=0	" turn off scrolling (faster)
set ttyfast			" we have a fast terminal connection

" Wrapping
set nowrap		" disable wrapping (cause its bad for coding)
set textwidth=0	" after how many chars a break is made
"set linebreak	" wrap on word boundaries (not in word middle)
"set showbreak=+	" show a + if a line is longer than the screen
"set cpoptions+=n	" show break char between line numbers

" Folding
set foldmethod=syntax	" fold mode - by syntax. control: zo, zc
set foldnestmax=5		" max. nested folds
set foldenable			" enable autofolding
set foldlevel=5			" Level from which autofold
set foldcolumn=1		" columns for fold markers
"set foldmarker={,}	" Fold C style code (only use if you use high f. level)
"set foldopen-=search	" don't unfold while searching
"set foldopen-=undo		" don't unfold when undo sth.

" Improvement & tweaks
set nojoinspaces 	" don't insert 2 spaces after .?! if 2 lines get j.
set undolevels=1000	" number of undo levels
set confirm			" confirm actions
set gdefault		" set 'g' flag by default with :s/foo/bar/
set autowrite		" automatically save before :next, :make etc.
"set list			" shows whether tabs or spaces are used
"set listchars=tab:>-,trail:·,eol:$, " which chars to show for tabs etc.
set listchars+=extends:\\,precedes:\\	" show not wrapped long lines
set nostartofline	" don't jump to first char with page commands
set nohidden		" Don't make me miss changes in hidden buffers
set report=0		" if lines got changed - notify
"set virtualedit=all	" let cursor move around beyond text
"set ofu=syntaxcomplete#Complete	"Insert mode tab completion (builtin)

" Status line & appearance
"set title			" shows title text in console title bar
"set statusline=	" don't need it
set t_Co=256		" 256 Color mode
set cmdheight=1		" command bar is 2 high
set showcmd			" show (partial) command in status line
set showmode		" show current mode
set ruler			" show cursor position in status line
set modeline		" enable modeline
set laststatus=2	" when to show a statusline
set number			" show line number
set numberwidth=4	" hopefully I never have more than 9999 lines per file
set lazyredraw		" no window redraw while running macros (faster)
set wildmenu		" turn on wild menu (completition feature)
set wildmode=longest:list	" turn on long format
set wildignore=*.dll,*.o,*.obj,*.exe,*.swp,*.jpg	" ignore some formats
colorscheme ir_black 	" my colorscheme for console mode
set background=dark		" terminal background
if has("gui_running")
	colorscheme wombat	" GUI scheme
	set cursorline	    " highlight current line
	set guifont=Monaco  " Font for GVim
	"set guioptions-=T	" Remove toolbar in GUI mode
	"set guioptions=acg	" Remove all menus, toolbars and stuff
	"set nomousehide	" don't hide mouse while typing
	"set lines=25		" standard console size
	"set columns=80		" standard console size
endif

" Anti annoyance
set vb t_vb=		" prevent annoying beeps, flash screen instead
set novisualbell	" don't blink
set noerrorbells	" no beeps
set shortmess=aoOstI	" shortens messages to avoid "press a key" prompt
set clipboard+=unnamed	" yank & copy to X clipboard

let mapleader = ","	" Set leader key (e.g. for NERDCommenter)

" Load plugins from bundle folders with pathogen
call pathogen#runtime_append_all_bundles()

" Enable syntax highlighting
if has("syntax")
	syntax on
endif
set popt+=syntax:y	" Syntax when printing

" Language specific stuff
if has("autocmd")
  " Enabled file type detection and file-type specific plugins.
  filetype plugin indent on " filetype plugin and indent
  autocmd FileType ruby setlocal expandtab shiftwidth=2 softtabstop=2	"ruby specific
endif

"Compile without make file and run
map <F6>	:!cc % & ./a.out<cr>
"Write to file without previous write access (asks password)
cmap w!! %!sudo tee > /dev/null %

" Switch current directory to that of the file
cmap cwd lcd %:p:h

"Highlight tabs
" syntax match Tab /\t/
" hi Tab guifg=grey ctermbg=grey

"Easy tab mode
"CTRL-T open tab CTRL-J focus left tab CTRL-K focus right tab
"set showtabline=2    " always show tab bar
set tabpagemax=20    " maximum number of tabs to create
map <C-t>	:tabnew<cr>
map <C-j>	:tabprevious<cr>
map <C-k>	:tabnext<cr>

"Sort a file
map ,s :%!sort<cr>
"Wrap on/off
map ,w :set wrap!<cr>
"apply ruby one liner to a range ($_ = current line)
map ,r :rubydo $_ = 
"Save and run ruby interpreter
map ,R :wall!<cr>:!ruby %<cr>

" Remove trailing whitespace from code files on save
function StripTrailingWhitespace()
  " store current cursor location
  silent exe "normal ma<CR>"
  " delete the whitespace (e means don't warn if pattern not found)
  %s/\s\+$//e
  " restore old cursor location
  silent exe "normal `a<CR>"
endfunction
au BufWritePre *.c,*.h,*.rb call StripTrailingWhitespace()

" when we reload, tell vim to restore the cursor to the saved position
augroup JumpCursorOnEdit
 au!
 autocmd BufReadPost *
 \ if expand("<afile>:p:h") !=? $TEMP |
 \ if line("'\"") > 1 && line("'\"") <= line("$") |
 \ let JumpCursorOnEdit_foo = line("'\"") |
 \ let b:doopenfold = 1 |
 \ if (foldlevel(JumpCursorOnEdit_foo) > foldlevel(JumpCursorOnEdit_foo - 1)) |
 \ let JumpCursorOnEdit_foo = JumpCursorOnEdit_foo - 1 |
 \ let b:doopenfold = 2 |
 \ endif |
 \ exe JumpCursorOnEdit_foo |
 \ endif |
 \ endif
 " Need to postpone using "zv" until after reading the modelines.
 autocmd BufWinEnter *
 \ if exists("b:doopenfold") |
 \ exe "normal zv" |
 \ if(b:doopenfold > 1) |
 \ exe "+".1 |
 \ endif |
 \ unlet b:doopenfold |
 \ endif
augroup END

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
