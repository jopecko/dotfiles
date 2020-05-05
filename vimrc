" http://dougblack.io/words/a-good-vimrc.html
execute pathogen#infect()
syntax enable                          " enable syntax processing
filetype plugin indent on              " file type-based indentation

set backspace=indent,eol,start

" Spaces & Tabs
set tabstop=4                          " number of visual spaces per TAB
set shiftwidth=4                       " affects how automatic indentation works with >>, << or ==
set softtabstop=4                      " number of spaces in tab when editing
set expandtab                          " tabs are spaces

" UI Config
set number                             " show line numbers
set showcmd                            " show commnd in bottom bar
set cursorline                         " highlight current line
filetype indent on                     " load filetype-specific indent files
set wildmenu                           " visual autocomplete for command menu
set lazyredraw                         " redraw only when need to
set showmatch                          " highlight matching [{()}]
set incsearch                          " search as characters are entered
set hlsearch                           " highlight matches

" <Ctrl-l> redraws the screen and removes any search highlighting.
nnoremap <silent> <C-l> :nohl<CR><C-l>

" Folding
set foldenable                         " enable folding
set foldlevelstart=10                  " open most folds by default
set foldnestmax=10                     " 10 nested fold max
" space open/closes folds
nnoremap <space> z
set foldmethod=indent                  " fold based on indent level

" Movement
" move vertically by visual line
nnoremap j gj
nnoremap k gk


set sm
set ai
let java_highlight_all=1
let java_highlight_functions="style"
let java_allow_coo_keywords=1
