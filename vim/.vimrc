"custom vimrc by eccentricayman on Github
set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

Plugin 'kien/ctrlp.vim'
Plugin 'godlygeek/tabular'
Plugin 'plasticboy/vim-markdown'
Plugin 'majutsushi/tagbar'
Plugin 'bling/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'sjl/gundo.vim'
Plugin 'rking/ag.vim'
Plugin 'scrooloose/syntastic'
Plugin 'tpope/vim-fugitive'
Plugin 'https://github.com/scrooloose/nerdtree'
Plugin 'bronson/vim-trailing-whitespace'
Plugin 'airblade/vim-gitgutter'
Plugin 'rkitover/vimpager'
Plugin 'edkolev/tmuxline.vim'
Plugin 'christoomey/vim-tmux-navigator'
Plugin 'keith/tmux.vim'
Plugin 'lilydjwg/colorizer'
Plugin 'tomtom/tcomment_vim'
Plugin 'vim-scripts/cSyntaxAfter'
Plugin 'ervandew/supertab'
Plugin 'Raimondi/delimitMate'
Plugin 'mhinz/vim-startify'
Plugin 'sheerun/vim-polyglot'
Plugin 'joshdick/onedark.vim'
Plugin 'alvan/vim-closetag'
Plugin 'easymotion/vim-easymotion'
Plugin 'haya14busa/incsearch.vim'
Plugin 'haya14busa/incsearch-easymotion.vim'


call vundle#end()            " required
filetype plugin indent on    " required

" General Confiration
" .....................................
syntax on            	" this is needed to see syntax
set background=dark  	" makes it easier to read with black background
set ls=2            	" allways show status line
set hlsearch        	" highlight searches
set incsearch       	" do incremental searching
set ruler           	" show the cursor position all the time
set noeb vb t_vb=     "autostart and no bells, visual or beep
au GUIEnter * set vb t_vb=
set ignorecase        " ignore case while searching
set number            " put numbers on side
set ts=4							" set the tabs to two spaces
set expandtab
set shiftwidth=4
set tabstop=4
set softtabstop=4
set backspace=indent,eol,start
set completeopt=longest,menu,preview
set autoindent
if has("gui_running")
    set scrolloff=5               " keep at least 5 lines above/below in GUI
endif
set cursorline                " highlight current line
hi clear CursorLine           " clear cursor line highlight
augroup CLClear
    autocmd! ColorScheme * hi clear CursorLine
augroup END
hi CursorLineNR cterm=bold
augroup CLNRSet               " highlight line number
    autocmd! ColorScheme * hi CursorLineNR cterm=bold
augroup END
set sidescrolloff=5           " keep at least 5 lines left/right
set hidden                    " this will go along"
set showcmd
filetype plugin on
filetype plugin indent on
let mapleader = ","           " Remap leader to comma
"let base16colorspace=256
set guioptions-=m  "remove menu bar
set guioptions-=T  "remove toolbar
set guioptions-=r  "remove right-hand scroll bar
set guioptions-=L  "remove lefthand scrollbaru
set guifont=Monaco\ for\ Powerline:h12
if has("gui_running")
  " GUI is running or is about to start.
  " Set gVim to a decent size
  set lines=41 columns=130
endif
inoremap jk <esc>
noremap <Up> <NOP>
noremap <Down> <NOP>
noremap <Left> <NOP>
noremap <Right> <NOP>
" <Leader>f{char} to move to {char}
map  <Leader>f <Plug>(easymotion-bd-f)
nmap <Leader>f <Plug>(easymotion-overwin-f)
" incsearch + easymotion
function! s:incsearch_config(...) abort
  return incsearch#util#deepextend(deepcopy({
  \   'modules': [incsearch#config#easymotion#module({'overwin': 1})],
  \   'keymap': {
  \     "\<CR>": '<Over>(easymotion)'
  \   },
  \   'is_expr': 0
  \ }), get(a:, 1, {}))
endfunction
noremap <silent><expr> /  incsearch#go(<SID>incsearch_config())
noremap <silent><expr> ?  incsearch#go(<SID>incsearch_config({'command': '?'}))
noremap <silent><expr> g/ incsearch#go(<SID>incsearch_config({'is_stay': 1}))
set wildmode=longest,list,full
set wildmenu
set lazyredraw
let delimitMate_expand_cr = 1

" Color Themes
" .....................................
colorscheme onedark
if has('gui_running')
   colorscheme onedark
endif
autocmd! FileType c,cpp,java,php call CSyntaxAfter()
let g:closetag_filenames = "*.html,*.xhtml,*.phtml"

" GitGutter
" .....................................
let g:gitgutter_enabled = 5
let g:gitgutter_highlights = 1
let g:gitgutter_override_sign_column_highlight = 0

" Custom Mappings
" .....................................
" Control screen splits
map <Tab> <C-W>w
map <Bar> <C-W>v<C-W><Right>
map -     <C-W>s<C-W><Down>
" Easier pane navigation"
" Use ctrl-[hjkl] to select the active split!
nmap <silent> <c-k> :wincmd k<CR>
nmap <silent> <c-j> :wincmd j<CR>
nmap <silent> <c-h> :wincmd h<CR>
nmap <silent> <c-l> :wincmd l<CR>
" Toggle Gundo
nnoremap <leader>u :GundoToggle<CR>
" Shortcut for Silver Searcher
nnoremap <leader>a :Ag
" F5 to delete all trailing whitespaces
nnoremap <silent> <F5> :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar>:nohl<CR>
map <C-Tab> :bnext<CR>
map <C-S-Tab> :bprev<CR>
nmap <F8> :TagbarToggle<CR>
" toggle gundo
nnoremap <leader>u :GundoToggle<CR>
" move vertically by visual line
nnoremap j gj
nnoremap k gk

" Custom Functions
" .....................................
function! Ccompilerun ()
    ! gcc %
    ! ./a.out
endfunction

nnoremap <leader>cc :call Ccompilerun()<CR>

function! Pythonrun ()
    ! python %
endfunction

nnoremap <leader>pp :call Pythonrun()<CR>

" cannot get this working :(
" function! Javacompilerun ()
"     ! javac %:t
"     ! java %:t:r
" endfunction

" nnoremap <leader>jj :call Javacompilerun()<CR>

function! Scriptrun ()
    ! sh %
endfunction

nnoremap <leader>ss :call Scriptrun()<CR>

" FileTypes
" .....................................
au BufRead,BufNewFile *.md set filetype=markdown

" Vim & Tmux
" .....................................
map <Esc>[B <Down>
if &term =~ '^screen'
    " tmux will send xterm-style keys when its xterm-keys option is on
    execute "set <xUp>=\e[1;*A"
    execute "set <xDown>=\e[1;*B"
    execute "set <xRight>=\e[1;*C"
    execute "set <xLeft>=\e[1;*D"
endif

" NERDTree
" .....................................
"autocmd VimEnter * NERDTree " Auto-start nerdtree and move focus to main pane
" Auto-close nerdtree when closing primary file
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif

autocmd VimEnter * wincmd p " Open help window on the left
autocmd FileType help wincmd L
let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""' " Make CtrlP faster by using Ag
"autoquits nerdtree if last buffer
function! NERDTreeQuit()
  redir => buffersoutput
  silent buffers
  redir END
"                     1BufNo  2Mods.     3File           4LineNo
  let pattern = '^\s*\(\d\+\)\(.....\) "\(.*\)"\s\+line \(\d\+\)$'
  let windowfound = 0

  for bline in split(buffersoutput, "\n")
    let m = matchlist(bline, pattern)

    if (len(m) > 0)
      if (m[2] =~ '..a..')
        let windowfound = 1
      endif
    endif
  endfor

  if (!windowfound)
    quitall
  endif
endfunction
let NerdTreeShowHidden=1
autocmd WinEnter * call NERDTreeQuit()

" CSS Color Configuraiton
" .....................................
let g:cssColorVimDoNotMessMyUpdatetime = 1

" Airline Configuration
" .....................................
let g:airline#extensions#tabline#enabled = 1 " Enable the list of buffers
let g:airline#extensions#tabline#fnamemod = ':t' " Show just the filename
let g:airline#extensions#branch#enabled = 1 " Enable branches
let g:airline_theme='onedark'

" Status Line Configuration
" .....................................
let g:airline_powerline_fonts = 1
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*
set statusline+=%{fugitive#statusline()}

" Syntastic Configuration
" .....................................
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_error_symbol = '✖'
let g:syntastic_style_error_symbol = '✖'
let g:syntastic_warning_symbol = '➜'
let g:syntastic_style_warning_symbol = '➜'

" Color Changes
" .....................................
hi LineNr                    ctermfg=green   ctermbg=black
hi NERDTreeCWD               ctermfg=black
hi NERDTreeLink              ctermfg=cyan
hi NERDTreeExecFile          ctermfg=green
hi SyntasticStyleWarningSign ctermfg=yellow  ctermbg=black
hi SyntasticStyleErrorSign   ctermfg=red     ctermbg=black
hi SyntasticWarningSign      ctermfg=yellow  ctermbg=black
hi SyntasticErrorSign        ctermfg=red     ctermbg=black
hi SignColumn                ctermbg=black
hi GitGutterAdd              ctermfg=green   ctermbg=black
hi GitGutterChange           ctermfg=yellow  ctermbg=black
hi GitGutterDelete           ctermfg=red     ctermbg=black
hi GitGutterChangeDelete     ctermfg=blue    ctermbg=black
hi vertsplit                 ctermfg=green   ctermbg=green
hi Pmenu                                     ctermbg=238
hi clear SignColumn

" my stufs
if has("gui_running")
else
    highlight clear LineNr
    highlight clear GitGutterAdd
    highlight clear GitGutterChange
    highlight clear GitGutterDelete
    highlight clear GitGutterChangeDelete
endif
