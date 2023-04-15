""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                NEOVIM INI / Peter Hofmann
"
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" GLOBAL
"
" ------------------------------------------------------------
" System IO "

" Set system clipboard as default register
set clipboard=unnamed

" Can use mouse to point and select
" i = insert
" v = visual
" a = all modes
set mouse=a

" Ignore uppercase and lowercase letters for searches
" \c makes the pattern ignore case
" \C makes the pattern match case
set ignorecase

" Overrides the `ignorecase` if the search pattern contains upper case chars
set smartcase

" ------------------------------------------------------------
" Backups / History "

" Set directory for saving history undo history
  " Neovim default is ~/.local/share/nvim/undo//
set undodir=~/.config/nvim/undo

" " Automatically saves an action history for `undo', even if vim is closed
set undofile


" " Starts the backup
" set backup

" " Sets the directory for the backups
" set backupdir=~/.config/nvim/backups

" " Files of which backups should be ignored
" set backupskip=/tmp/*

" " Saves the file to backup
" set writebackup

" " Setting for hot reload (disable or safe write)
" set backupcopy=yes

" ------------------------------------------------------------
" Key mappings "

" Remap esc-key to jk
inoremap jk <esc>
nnoremap <C-g> <esc>

" Shift lines up/down
noremap <C-S-up> :m -2<CR>
noremap <C-S-down> :m +1<CR>
" silent! call repeat#set("\<Plug>MyWonderfulMap", v:count)


" Leader Key: https://tuckerchapman.com/2018/06/16/how-to-use-the-vim-leader-key/

" Remap leader-key to space
let mapleader = "\<Space>"
let maplocalleader = ","

" Set leader-key timeout duration:
" By default timeoutlen is 1000 ms
set timeoutlen=1000 " 500

set showcmd

" Searches globally
" :map <leader>s :s//g<left><left>

" insert new line above/below without entering insert mode
nmap <Leader>[ O<Esc>
nmap <Leader>] o<Esc>

" Unload current buffer
nnoremap <Leader>q :bd<CR>

" Open terminal buffer in insert mode
autocmd TermOpen * startinsert

" map <leader>h :noh<CR>
map § :noh<CR>

" Exit terminal mode
" tnoremap <Leader><Esc> <C-\><C-n> " slows down input
" tnoremap <Leader><Tab> <C-\><C-n> " slows down input
tnoremap § <C-\><C-n>

" Zoom/restore a window
" source: https://vi.stackexchange.com/a/1902
function! s:ZoomToggle() abort
  if exists('t:zoomed') && t:zoomed
    execute t:zoom_winrestcmd
    let t:zoomed = 0
  else
    let t:zoom_winrestcmd = winrestcmd()
    resize
    vertical resize
    let t:zoomed = 1
  endif
endfunction

command! ZoomToggle call s:ZoomToggle()

nnoremap <leader>a :ZoomToggle<CR>

" ------------------------------------------------------------
" Misc settings "

" For syntqx highlighting
  " (default in Neovim)
" syntax on  " also called 'syntax enable'
" filetype plugin indent on

" Set relative numbered lines
set number
set relativenumber

" minimum number of lines above and below the cursor
set scrolloff=5

" Yank highlighting
"

lua <<EOF
vim.api.nvim_create_autocmd({"TextYankPost"}, {
  callback = function() vim.highlight.on_yank({higroup = 'IncSearch', timeout = 200, on_visual = false}) end,
})
EOF
"
" if has("nvim")
"   augroup highlight_my_yank
"       autocmd!
"       au TextYankPost * silent! lua vim.highlight.on_yank{higroup="IncSearch", timeout=200, on_visual=false}
"       " au TextYankPost * silent! lua vim.highlight.on_yank{higroup="IncSearch", timeout=200}
"   augroup END
" endif
" au TextYankPost * silent! lua vim.highlight.on_yank{higroup="IncSearch", timeout=150}


" Display a column to mark 80 spaces
set colorcolumn=80

" Display hidden characters
set list
" Does not display hidden symbols by default
set list!
" Toggle to display / hide hidden symbols
nnoremap <leader>l :set list!<cr>

" Defines how the hidden symbols will be represented
set listchars=tab:▸\
set listchars+=trail:·
set listchars+=eol:↴
set listchars+=nbsp:_

" It may slow nvim down, but guaranteed that syntax highlight will always work
" autocmd BufEnter * :syntax sync fromstart

" Allows you to have a preview before doing a substitution with :%s
" set inccommand=split

" No linewrap
" set nowrap

" Allows you to edit other files without saving one before opening another
" (already defaut in Neovim)
" set hidden

if has("nvim")
  set inccommand=nosplit " show substitutions incrementally
endif

" ------------------------------------------------------------
" Window settings "

" Split new (split) window to the right of current
set splitright
" Split new (vsplit) window below current
set splitbelow

" Splits to empty window
nnoremap <leader>ws :new<CR>
nnoremap <leader>wv :vnew<CR>

" Closes the window without closing the last one
nnoremap <leader>wc :clo<CR>

" set windows equal vertically
nmap <leader>- :set ead=ver ea noea<CR>
" set windows equal horizontally
nmap <leader>\ :set ead=hor ea noea<CR>
" set windows full vertically
nmap <leader>_ :res<CR>
" set windows full horizontally
nmap <leader>\| :vert res<CR>

" ------------------------------------------------------------
" Indent settings "

" see: http://vimcasts.org/episodes/tabs-and-spaces/


" Use space characters instead of tabs.
set expandtab

" Set space width from tab key to n spaces.
set tabstop=2

" Set shift width for indentation commands (e.g. >>/<<) to n spaces.
set shiftwidth=2

" Fine-tune amount of whitespace for tab key insertions.
" (e.g. delete n spaces with DEL key from a tab insert)
set softtabstop=2

" Automatically indent next line from indented line.
"   (default in nvim)
" set autoindent

" Set space width on line start to shiftwidth (ignoring softtabstop)
"   (unnecessary if shiftwidth == softtabstop)
"   (default in nvim)
" set smarttab

" ------------------------------------------------------------
" True Colors "

"Use 24-bit (true-color) mode in Vim/Neovim when outside tmux.
"If you're using tmux version 2.2 or later, you can remove the outermost $TMUX check and use tmux's 24-bit color support
"(see < http://sunaku.github.io/tmux-24bit-color.html#usage > for more information.)
if (empty($TMUX))
  if (has("nvim"))
    "For Neovim 0.1.3 and 0.1.4 < https://github.com/neovim/neovim/pull/2198 >
    let $NVIM_TUI_ENABLE_TRUE_COLOR=1
  endif
  "For Neovim > 0.1.5 and Vim > patch 7.4.1799 < https://github.com/vim/vim/commit/61be73bb0f965a895bfb064ea3e55476ac175162 >
  "Based on Vim patch 7.4.1770 (`guicolors` option) < https://github.com/vim/vim/commit/8a633e3427b47286869aa4b96f2bfc1fe65b25cd >
  " < https://github.com/neovim/neovim/wiki/Following-HEAD#20160511 >
  if (has("termguicolors"))
    set termguicolors
  endif
endif

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" PLUGINS: vim-plug
"

" disable LSP features in ALE to prevent conflicts with coc.nvim
let g:ale_disable_lsp = 1 

" function general#Plugin_loaded(plugin)
"   return isdirectory(g:plug_home . "/" . a:plugin . ".vim")
"     \ && has_key(g:plugs, a:plugin . '.vim')
" endfunction

" Plugin Tips:
" :echo g:plugs_order  to check which plugins are installed (in order)
" :scriptnames  to see if a plugin is loaded

call plug#begin()

" Plug 'neovim/nvim-lspconfig'
Plug 'neoclide/coc.nvim', {'branch': 'release'}  " Auto Completion
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
" Plug 'dense-analysis/ale' " LSP diagnostics (linter)
" Plug 'kassio/neoterm' " Terminal and REPL

Plug 'preservim/tagbar' " Tagbar for code navigation
" Plug 'liuchengxu/vim-which-key' " port of emacs-which-key
Plug 'vim-airline/vim-airline' " Status bar
Plug 'vim-airline/vim-airline-themes'
Plug 'airblade/vim-gitgutter' " see git changes on left gutter
" Plug 'ethanholz/nvim-lastplace' " like farmergreg/vim-lastplace but in Lua
    " -> doesn't work for some reason
Plug 'farmergreg/vim-lastplace' " saves and restores last line of edit 
Plug 'lukas-reineke/indent-blankline.nvim' " indentation lines (+ on blank lines)
Plug 'inkarkat/vim-ReplaceWithRegister'
" Plug 'yuttie/comfortable-motion.vim' " smooth scrolling with u/d
Plug 'simnalamburt/vim-mundo' " lists undo history to retrieve lost changes
Plug 'vim-scripts/YankRing.vim' " list/cycle clipboard history
Plug 'michaeljsmith/vim-indent-object' " indentation text object
Plug 'tpope/vim-repeat' " enables repeating (.) commands after plugin maps
Plug 'ggandor/leap.nvim' " jumping around in the document

Plug 'p00f/nvim-ts-rainbow' " Rainbow brackets with treesitter support
" Plug 'junegunn/rainbow_parentheses.vim' " doesn't work??
" Plug 'luochen1990/rainbow'
" Plug 'markonm/hlyank.vim'

" Like Org-Mode for Neovim (needs Tree Sitter - wait for nvim 0.6):
" Plug 'nvim-neorg/neorg' | Plug 'nvim-lua/plenary.nvim'

" File Explorer
" Plug 'preservim/nerdtree' " File browser
Plug 'kyazdani42/nvim-web-devicons' " for file icons
Plug 'kyazdani42/nvim-tree.lua'
" Plug 'lambdalisue/fern.vim' " another tree browser
Plug 'christoomey/vim-tmux-navigator' " navigate between panes using shortcuts
    " <C-h> to jump to left
    " <C-l> to jump to right
" Plug 'ctrlpvim/ctrlp.vim' " fuzzy find files (use fzf instead)
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } } " fuzzyfind
Plug 'junegunn/fzf.vim'
    " See https://dev.to/iggredible/how-to-search-faster-in-vim-with-fzf-vim-36ko

" Language-specific plugins
Plug 'rust-lang/rust.vim'
Plug 'hellerve/carp-vim'
Plug 'bakpakin/janet.vim'
Plug 'rescript-lang/vim-rescript'
Plug 'nkrkv/nvim-treesitter-rescript'
" Plug 'clojure-vim/clojure.vim' " needed?
Plug 'datwaft/prolog-syntax-vim'
" --- experimental ---
" Plug 'tpope/vim-fireplace'
" Plug 'guns/vim-clojure-static'
" Plug 'guns/vim-clojure-highlight' " requires vim-clojure-static & vim-fireplace
" --------------------
Plug 'vlime/vlime', {'rtp': 'vim/'} " Common-Lisp server like Slime in Emacs
Plug 'Olical/conjure' " Clojure dev (use vim-iced instead)
" Conjure stuff (backup):
  " Plug 'Olical/aniseed' " Lisp Config (compiles to Lua) for Neovim
  " Jack-in for clojure
  " Plug 'tpope/vim-dispatch'
  " Plug 'clojure-vim/vim-jack-in' " depends on vim-dispatch
  " Plug 'radenling/vim-dispatch-neovim' " (only in Neovim)
Plug 'guns/vim-sexp', {'for': ['clojure', 'janet', 'carp', 'scheme', 'fennel', 'lisp']}
Plug 'tpope/vim-sexp-mappings-for-regular-people' " because meta-key
Plug 'liquidz/vim-iced', {'for': 'clojure'} " clojure dev (depends on vim-sexp)
Plug 'liquidz/vim-iced-coc-source', {'for': 'clojure'} " coc plugin for vim-iced
Plug 'lambdalisue/fern.vim' " required for vim-iced-fern-debugger
Plug 'liquidz/vim-iced-fern-debugger', {'for': 'clojure'} " debugger for vim-iced

" Git
" Plug 'Xuyuanp/nerdtree-git-plugin'
" Plug 'tpope/vim-fugitive'
" Plug 'tsony-tsonev/nerdtree-git-plugin'
    " seems to do the same as nerdtree-git-plugin
" Plug 'tiagofumo/vim-nerdtree-syntax-highlight'
    " seems to work with nerdtree-git-plugin

Plug 'tpope/vim-surround' " Surrounding ysw)
Plug 'tpope/vim-commentary' " For Commenting gcc & gc
" Plug 'jbgutierrez/vim-better-comments' " Color coding of comments (!, ?, …)
" -> does not work with TreeSitter :(
" Plug 'terryma/vim-multiple-cursors' " (deprecated!) CTRL + N for multiple cursors
Plug 'mg979/vim-visual-multi', {'branch': 'master'}
Plug 'ap/vim-css-color' " CSS Color Preview
" -- paranthesis balancing in Lisps already managed by guns/vim-sexp
" -- this can be diabled with:
"    let g:sexp_enable_insert_mode_mappings = 0
" Plug 'bhurlow/vim-parinfer'
Plug 'gpanders/nvim-parinfer'
" Plug 'jiangmiao/auto-pairs' " Automatically set closing parenthesis
" Plug 'editorconfig/editorconfig-vim' " Consistent editor configs: https://editorconfig.org/

" Syntax
Plug 'HerringtonDarkholme/yats.vim' " TS Syntax
" Plug 'chrisbra/csv.vim' " CSV table editing
Plug 'neovimhaskell/haskell-vim' " Better Haskell syntax highlighting
" Plug 'sheerun/vim-polyglot' " Better syntax highlighting

" Completion
" Plug 'mattn/emmet-vim'

" Themes
" Plug 'rafi/awesome-vim-colorschemes' " Retro Scheme
" Plug 'folke/tokyonight.nvim', { 'branch': 'main' }
" Plug 'EdenEast/nightfox.nvim'
" Plug 'drewtempelmeyer/palenight.vim'
" Plug 'frenzyexists/aquarium-vim', { 'branch': 'develop' }
" Plug 'franbach/miramare' " doesn't work for some reason
  " these are okay:
" Plug 'ellisonleao/gruvbox.nvim' " treesitter support (not really better)
Plug 'morhetz/gruvbox'
Plug 'challenger-deep-theme/vim', { 'as': 'challenger-deep' }
Plug 'embark-theme/vim', { 'as': 'embark', 'branch': 'main' } " <- great one
Plug 'haishanh/night-owl.vim'
Plug 'joshdick/onedark.vim'
Plug 'drewtempelmeyer/palenight.vim'
Plug 'rakr/vim-one'
Plug 'arcticicestudio/nord-vim'
Plug 'sainnhe/sonokai'
Plug 'dracula/vim', { 'as': 'dracula' }

Plug 'savq/melange'
Plug 'AhmedAbdulrahman/vim-aylin'
" Plug 'mcchrish/zenbones.nvim'
Plug 'fcpg/vim-farout'
Plug 'fenetikm/falcon' " could be ok if background color was softer
" Plug 'danilo-augusto/vim-afterglow'
Plug 'foxbunny/vim-amber'
Plug 'rose-pine/neovim'


" Plug 'ryanoasis/vim-devicons' " Developer Icons (--> now nvim-devicons)
" Plug 'https://github.com/tc50cal/vim-terminal' " Vim Terminal --> needs some
" Python stuff (?)

call plug#end()

" ------------------------------------------------------------
" THEMES

" let g:tokyonight_style = "night"

" set background=dark

let g:gruvbox_italic = 1
let g:gruvbox_italicize_comments = 1
let g:gruvbox_contrast_dark = 'hard'

let g:embark_terminal_italics = 1

let g:miramare_enable_italic = 1
let g:miramare_disable_italic_comment = 1

let g:palenight_terminal_italics=1

let g:sonokai_style='shusia' " 'default' 'atlantis' 'andromeda' 'shusia' 'maia' 'espresso'
" let g:sonokai_better_performance = 1
let g:sonokai_enable_italic = 1

" let g:palenight_color_overrides = {
" \    'black': { 'gui': '#242837', "cterm": "0", "cterm16": "0" },
" \}

" if (g:colors_name == 'farout')
augroup customFarout
  au!
  autocmd ColorScheme * hi IndentBlanklineChar guifg=#41251e
  autocmd ColorScheme * hi CocUnusedHighlight guibg=#1F1311
  " autocmd ColorScheme * hi CocUnusedHighlight guibg=None guifg=#993923
  autocmd ColorScheme * hi MatchParen guibg=#6B4035
augroup END
" endif

colorscheme melange  "embark farout sonokai palenight nightfox  tokyonight  jellybeans  gruvbox

" ------------------------------------------------------------
" PLUGIN: airline

let g:airline_powerline_fonts = 1
let g:airline_theme='atomic' "embark farout sonokai 

" ------------------------------------------------------------
" PLUGIN: tagbar

nnoremap <silent> <leader>s :TagbarToggle<CR>

" ------------------------------------------------------------
" PLUGIN: vim-visual-multi



" ------------------------------------------------------------
" PLUGIN: haskell-vim

let g:haskell_enable_quantification = 1   " to enable highlighting of `forall`
let g:haskell_enable_recursivedo = 1      " to enable highlighting of `mdo` and `rec`
let g:haskell_enable_arrowsyntax = 1      " to enable highlighting of `proc`
let g:haskell_enable_pattern_synonyms = 1 " to enable highlighting of `pattern`
let g:haskell_enable_typeroles = 1        " to enable highlighting of type roles
let g:haskell_enable_static_pointers = 1  " to enable highlighting of `static`
let g:haskell_backpack = 1                " to enable highlighting of backpack keywords

" ------------------------------------------------------------
" PLUGIN: bnf.vim (see syntax/bnf.vim)

au bufreadpre,bufnewfile *.bnf set ft=bnf
"
" ------------------------------------------------------------
" PLUGIN: ebnf.vim (see syntax/bnf.vim)

au bufreadpre,bufnewfile *.ebnf set ft=ebnf

" ------------------------------------------------------------
" PLUGIN: hellerve/carp-vim

au FileType carp set lisp

" ------------------------------------------------------------
" PLUGIN: yankring

nnoremap <silent> <Leader>y :YRShow<CR>

" ------------------------------------------------------------
" PLUGIN: vim-mundo

nnoremap <silent> <Leader>u :MundoToggle<CR>

" ------------------------------------------------------------
" PLUGIN: fzf

nnoremap <silent> <Leader>b :Buffers<CR>
nnoremap <silent> <Leader>f :Files<CR>
nnoremap <silent> <Leader>r :Rg<CR>
nnoremap <silent> <Leader>/ :BLines<CR>
nnoremap <silent> <Leader>' :Marks<CR>
nnoremap <silent> <Leader>g :Commits<CR>
nnoremap <silent> <Leader>H :Helptags<CR>
nnoremap <silent> <Leader>hh :History<CR>
nnoremap <silent> <Leader>h: :History:<CR>
nnoremap <silent> <Leader>h/ :History/<CR> 
command! -bang -nargs=* Rg call fzf#vim#grep("rg --no-require-git --column --line-number --no-heading --color=always --smart-case ".shellescape(<q-args>), 1, {'options': '--delimiter : --nth 4..'}, <bang>0) " ignore filenames in Rg search

" fzf in :Buffers context (?)
    " see https://github.com/junegunn/fzf.vim/pull/733
let g:fzf_action = {
  \ 'ctrl-d': 'bd!',
  \ 'ctrl-t': 'tab split',
  \ 'ctrl-x': 'split',
  \ 'ctrl-v': 'vsplit' }

" command! -bang -nargs=* Rg
"   \ call fzf#vim#grep(
"   \   "rg -g '!.git' -g '!node_modules' --column --line-number --no-heading --color=always --smart-case ".shellescape(<q-args>), 1,
"   \   fzf#vim#with_preview({'options': '--exact --delimiter : --nth 4..'}), <bang>0)

" Do not display the file content preview window
" let g:fzf_preview_window = []

" Syntax Highlight and default configs for the preview window
" let $FZF_DEFAULT_OPTS="--preview-window 'right:60%' --layout reverse --margin=0,0 --preview 'bat --color=always --style=header,grid --line-range :300 {}'"

" Positioning of the FZF window
" let g:fzf_layout = 
" \ { 'window': 
"   \ { 'width': 0.98, 'height': 0.7, 'yoffset': 0.94, 'border': 'rounded' } 
" \ } 

" FZF colors must match the colors of the theme
" let g:fzf_colors =
" \ { 'fg':      ['fg', 'Normal'],
"   \ 'bg':      ['bg', 'Normal'],
"   \ 'hl':      ['fg', 'Comment'],
"   \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
"   \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
"   \ 'hl+':     ['fg', 'Statement'],
"   \ 'info':    ['fg', 'PreProc'],
"   \ 'border':  ['fg', 'Ignore'],
"   \ 'prompt':  ['fg', 'Conditional'],
"   \ 'pointer': ['fg', 'Exception'],
"   \ 'marker':  ['fg', 'Keyword'],
"   \ 'spinner': ['fg', 'Label'],
"   \ 'header':  ['fg', 'Comment'] }

" ------------------------------------------------------------
" PLUGIN: vim-which-key

" call which_key#register('<Space>', "g:which_key_map")

" " nnoremap <silent> <leader> :WhichKey '<Space>'<CR>
" nnoremap <silent> <leader>      :<c-u>WhichKey '<Space>'<CR>
" nnoremap <silent> <localleader> :<c-u>WhichKey  ','<CR>

" let g:which_key_map =  {}
" let g:which_key_map.w = { 'name' : 'Windows' }

" ------------------------------------------------------------
" PLUGIN: leap.nvim

lua require('leap').set_default_keymaps()

" ------------------------------------------------------------
" PLUGIN: nvim-neorg

" lua << EOF
"     require('neorg').setup {
"         -- Tell Neorg what modules to load
"         load = {
"             ["core.defaults"] = {}, -- Load all the default modules
"             ["core.norg.concealer"] = {}, -- Allows for use of icons
"             ["core.norg.dirman"] = { -- Manage your directories with Neorg
"                 config = {
"                     workspaces = {
"                         my_workspace = "~/Documents/neorg"
"                     }
"                 }
"             }
"         },
"     }
" EOF

" ------------------------------------------------------------
" PLUGIN: nvim-web-devicons

lua require('nvim-web-devicons-config')

" ------------------------------------------------------------
" PLUGIN: nvim-tree

nnoremap <leader>t :NvimTreeToggle<CR>
nnoremap <localleader>r :NvimTreeRefresh<CR>
nnoremap <leader>n :NvimTreeFindFile<CR>
" NvimTreeOpen, NvimTreeClose, NvimTreeFocus, NvimTreeFindFileToggle, and NvimTreeResize are also available if you need them

set termguicolors " this variable must be enabled for colors to be applied properly

" a list of groups can be found at `:help nvim_tree_highlight`
highlight NvimTreeFolderIcon guibg=blue

lua require('nvim-tree-config')

" ------------------------------------------------------------
" PLUGIN: comfortable-motion

" let g:comfortable_motion_scroll_down_key = "j"
" let g:comfortable_motion_scroll_up_key = "k"

" ------------------------------------------------------------
" PLUGIN: commentary

" Comment lines using <leader> + /
nnoremap <leader>/ :Commentary<cr>
vnoremap <leader>/ :Commentary<cr>

" ------------------------------------------------------------
" PLUGIN: nerdtree (-> obsolete?)

" nnoremap <C-f> :NERDTreeFocus<CR>
" nnoremap <C-t> :NERDTree<CR> " C-n already used by multiple-cursors
" nnoremap <leader>t :NERDTreeToggle<CR>

" let g:NERDTreeDirArrowExpandable="▷"
" let g:NERDTreeDirArrowCollapsible="▽"
" " Automatically close NERDTree when you open a file
" " let NERDTreeQuitOnOpen=1

" " let g:NERDTreeIgnore = ['^node_modules$']

" let g:NERDTreeGitStatusWithFlags = 1
" " let g:WebDevIconsUnicodeDecorateFolderNodes = 1
" " let g:NERDTreeGitStatusNodeColorization = 1
" " let g:NERDTreeColorMapCustom = {
" "     \ "Modified"  : "#528AB3",  
" "     \ "Staged"    : "#538B54",  
" "     \ "Untracked" : "#BE5849",  
" "     \ "Dirty"     : "#299999",  
" "     \ "Clean"     : "#87939A",   
" "     \ "Ignored"   : "#808080"   
" "     \ } 

" ------------------------------------------------------------
" PLUGIN: ctrlp

" let g:ctrlp_user_command = ['.git/', 'git --git-dir=%s/.git ls-files -oc --exclude-standard']

" ------------------------------------------------------------
" PLUGIN: coc.nvim

inoremap <silent><expr> <TAB>
      \ coc#pum#visible() ? coc#_select_confirm() : "\<Tab>"

" Make <CR> to accept selected completion item or notify coc.nvim to format
" <C-g>u breaks current undo, please make your own choice.
" inoremap <silent><expr> <CR> coc#pum#visible() ? coc#pum#confirm()
"                               \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"

" Remap keys for gotos
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gI <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use gh to show documentation in preview window
nnoremap <silent> gh :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" Remap for rename current word
nmap <F2> <Plug>(coc-rename)

" CoC extensions:
    " https://github.com/neoclide/coc-snippets
        " :CocCommand snippets.editSnippets  -- to add/edit custom snippets
    " https://github.com/neoclide/coc-python
    " https://github.com/neoclide/coc-tsserver
    " https://github.com/neoclide/coc-eslint
    " https://github.com/neoclide/coc-json

" ------------------------------------------------------------
" PLUGIN: vim-rescript

" Note that <buffer> allows us to use different commands with the same keybindings depending
" on the filetype. This is useful if to override your e.g. ALE bindings while working on
" ReScript projects.
autocmd FileType rescript nnoremap <silent> <buffer> <localleader>r :RescriptFormat<CR>
autocmd FileType rescript nnoremap <silent> <buffer> <localleader>t :RescriptTypeHint<CR>
autocmd FileType rescript nnoremap <silent> <buffer> <localleader>b :RescriptBuild<CR>
autocmd FileType rescript nnoremap <silent> <buffer> gd :RescriptJumpToDefinition<CR>

" Hooking up the ReScript autocomplete function
set omnifunc=rescript#Complete

" When preview is enabled, omnicomplete will display additional
" information for a selected item
set completeopt+=preview

" ------------------------------------------------------------
" PLUGIN: nvim-treesitter

lua <<EOF
require'nvim-treesitter.configs'.setup {
  highlight = {
    enable = true,
    disable = { 'clojure', 'janet', 'carp', 'scheme', 'fennel', 'lisp' }, -- see https://github.com/guns/vim-sexp/issues/31
    custom_captures = {
      -- Highlight the @foo.bar capture group with the "Identifier" highlight group.
      -- ["foo.bar"] = "Identifier",
    },
    -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
    -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
    -- Using this option may slow down your editor, and you may see some duplicate highlights.
    -- Instead of true it can also be a list of languages
    additional_vim_regex_highlighting = false,
  },
  rainbow = { -- DOESNT WORK FOR SOME REASON!!!
    enable = false,
    -- disable = { "jsx", "cpp" }, list of languages you want to disable the plugin for
    extended_mode = true, -- Also highlight non-bracket delimiters like html tags, boolean or table: lang -> boolean
    max_file_lines = nil, -- Do not enable for files with more than n lines, int
    -- colors = {}, -- table of hex strings
    -- termcolors = {} -- table of colour name strings
  },
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = "gnn",
      node_incremental = "grn",
      scope_incremental = "grc",
      node_decremental = "grm",
    },
  },
}
EOF

" ------------------------------------------------------------
" PLUGIN: Neoterm

"alt + v, like in Idea
vnoremap <localleader>v :<c-u>exec v:count.'TREPLSendSelection'<cr>
" nnoremap <localleader>L :<c-u>exec v:count.'TREPLSendLine'<cr>
" nnoremap <localleader>B :<c-u>exec v:count.'TREPLSendFile'<cr>


" ------------------------------------------------------------
" PLUGIN: ale

let g:ale_linters = {'clojure': ['clj-kondo']}

" NOTE regarding upgrade borkdude/brew/clj-kondo (2022.03.09) < 2022.04.08:
" strange namespace error...
" disable linter in ~/.config/clj-kondo/config.edn :
" {:linters {:namespace-name-mismatch {:level :off}}}

nnoremap <silent> <localleader>§ :ALEToggleBuffer<CR>

" ------------------------------------------------------------
" PLUGIN: clojure.vim

let g:clojure_maxlines = 100

" ------------------------------------------------------------
" PLUGIN: vim-iced

" if general#Plugin_loaded('vim-iced')
if exists('g:plugs["vim-iced"]')

  " Disable linter for Iced buffer:
  " - see https://github.com/liquidz/vim-iced/issues/382
  "
  " call iced#hook#add('connected', {
  "       \ 'type': 'function',
  "       \ 'exec': {_ -> ale#toggle#DisableBuffer(bufnr('iced_stdout'))},
  "       \ })

  call iced#hook#add('connected', {
        \ 'type': 'function',
        \ 'exec': {_ -> CocAction('diagnosticToggleBuffer', bufnr('iced_stdout'))},
        \ })


  " Enable vim-iced's default key mapping
  " This is recommended for newbies
  let g:iced_enable_default_key_mappings = v:false
  " let g:iced_enable_clj_kondo_analysis = v:true
  " let g:iced_enable_clj_kondo_local_analysis = v:true

  let g:iced#buffer#stdout#mods = 'botright' " 'vertical'
  let g:iced_enable_auto_indent = v:true
  let g:iced#nrepl#skip_evaluation_when_buffer_size_is_exceeded = v:true

  " indent rules for cljfmt (used by vim-iced)
  " let g:iced#format#rule = {
  "     \ 'clojure.core.logic/run*': '[[:block 2] [:inner 1]]',
  "     \ }

  " Tonsky’s formatting rule
  " let g:iced#format#rule = {'#"^\w"': '[[:inner 0]]'}

  let g:iced#format#options = {
      \ 'remove-surrounding-whitespace?': v:false,
      \ 'insert-missing-whitespace?': v:true,
      \ 'remove-consecutive-blank-lines?': v:false,
      \ 'remove-multiple-non-indenting-spaces?': v:false,
      \ 'split-keypairs-over-multiple-lines?': v:false,
      \ }

  let g:iced#eval#keep_inline_result = v:true

  " mapping for "xee
  " nmap <silent> ee <Plug>(iced_eval)<Plug>(sexp_outer_list)``

  nmap <localleader>e' <Plug>(iced_eval_and_print)af

  nmap <localleader>ce <Plug>(iced_clear_inline_result)

  " ---------------
  " default keymaps
  " (remapped to localleader)

  nmap <Leader>' <Plug>(iced_connect)

    "" Evaluating (<Leader>e)
  nmap <localleader>eq <Plug>(iced_interrupt)
  nmap <localleader>eQ <Plug>(iced_interrupt_all)
  nmap <Leader>" <Plug>(iced_jack_in)

    "" if !hasmapto(<Plug>(iced_eval))
  nmap <localleader>ei <Plug>(iced_eval)<Plug>(sexp_inner_element)
  nmap <localleader>ee <Plug>(iced_eval)<Plug>(sexp_outer_list)
  nmap <localleader>et <Plug>(iced_eval_outer_top_list)
    "" endif

  nmap <localleader>ea <Plug>(iced_eval_at_mark)
  nmap <localleader>el <Plug>(iced_eval_last_outer_top_list)
  vmap <localleader>ee <Plug>(iced_eval_visual)
  nmap <localleader>en <Plug>(iced_eval_ns)
  nmap <localleader>ep <Plug>(iced_print_last)
  nmap <localleader>eb <Plug>(iced_require)
  nmap <localleader>eB <Plug>(iced_require_all)
  nmap <localleader>eu <Plug>(iced_undef)
  nmap <localleader>eU <Plug>(iced_undef_all_in_ns)
  nmap <localleader>eM <Plug>(iced_macroexpand_outer_list)
  nmap <localleader>em <Plug>(iced_macroexpand_1_outer_list)
  nmap <localleader>enr <Plug>(iced_refresh)
  nmap <localleader>ece <Plug>(iced_eval_in_context)<Plug>(sexp_outer_list)

    "" Testing (<Leader>t)
  nmap <localleader>tt <Plug>(iced_test_under_cursor)
  nmap <localleader>tl <Plug>(iced_test_rerun_last)
  nmap <localleader>ts <Plug>(iced_test_spec_check)
  nmap <localleader>to <Plug>(iced_test_buffer_open)
  nmap <localleader>tc <Plug>(iced_test_buffer_close)
  nmap <localleader>tn <Plug>(iced_test_ns)
  nmap <localleader>tp <Plug>(iced_test_all)
  nmap <localleader>tr <Plug>(iced_test_redo)

    "" Stdout buffer (<Leader>s)
  nmap <localleader>ss <Plug>(iced_stdout_buffer_toggle)
  nmap <localleader>sl <Plug>(iced_stdout_buffer_clear)
  nmap <localleader>so <Plug>(iced_stdout_buffer_open)
  nmap <localleader>sq <Plug>(iced_stdout_buffer_close)

    "" Refactoring (<Leader>r)
  nmap <localleader>rcn <Plug>(iced_clean_ns)
  nmap <localleader>rca <Plug>(iced_clean_all)
  nmap <localleader>ram <Plug>(iced_add_missing)
  nmap <localleader>ran <Plug>(iced_add_ns)
  nmap <localleader>rtf <Plug>(iced_thread_first)
  nmap <localleader>rtl <Plug>(iced_thread_last)
  nmap <localleader>ref <Plug>(iced_extract_function)
  nmap <localleader>raa <Plug>(iced_add_arity)
  nmap <localleader>rml <Plug>(iced_move_to_let)
  nmap <localleader>rrs <Plug>(iced_rename_symbol)

    "" Help/Document (<Leader>h)
  nmap K          <Plug>(iced_document_popup_open)
  nmap <localleader>hb <Plug>(iced_document_open)
  nmap <localleader>hu <Plug>(iced_use_case_open)
  nmap <localleader>hn <Plug>(iced_next_use_case)
  nmap <localleader>hN <Plug>(iced_prev_use_case)
  nmap <localleader>hq <Plug>(iced_document_close)
  nmap <localleader>hS <Plug>(iced_source_show)
  nmap <localleader>hs <Plug>(iced_source_popup_show)
  nmap <localleader>hc <Plug>(iced_clojuredocs_open)
  nmap <localleader>hh <Plug>(iced_command_palette)

    "" Browsing (<Leader>b)
  nmap <localleader>bn  <Plug>(iced_browse_related_namespace)
  nmap <localleader>bs  <Plug>(iced_browse_spec)
  nmap <localleader>bt  <Plug>(iced_browse_test_under_cursor)
  nmap <localleader>br  <Plug>(iced_browse_references)
  nmap <localleader>bd  <Plug>(iced_browse_dependencies)

    "" Jumping cursor (<Leader>j)
  nmap <C-]>           <Plug>(iced_def_jump)
  nmap <localleader>jn <Plug>(iced_jump_to_next_sign)
  nmap <localleader>jN <Plug>(iced_jump_to_prev_sign)
  nmap <localleader>jl <Plug>(iced_jump_to_let)

    "" Debugging (<Leader>d)
  nmap <localleader>dbt <Plug>(iced_browse_tapped)
  nmap <localleader>dlt <Plug>(iced_clear_tapped)

    "" Misc
  nmap ==              <Plug>(iced_format)
  nmap =G              <Plug>(iced_format_all)
  nmap <Leader>*       <Plug>(iced_grep)
  nmap <localleader>/  :<C-u>IcedGrep<Space>
  nmap <localleader>yn <Plug>(iced_yank_ns_name)

endif

" ------------------------------------------------------------
" PLUGIN: vim-surround

" Comment out a Clojure form
" - (from https://clojurians.slack.com/archives/C0DF8R51A/p1651138951103709?thread_ts=1638731389.109100&cid=C0DF8R51A):
let g:surround_99 = "#_\r"
nmap <Leader>c ysafc

" ------------------------------------------------------------
" PLUGIN: auto-pairs

" let g:AutoPairs = {'(':')', '[':']', '{':'}'}
" let g:AutoPairsMapCR = 0
" let g:AutoPairsMapSpace = 0

" " To disable auto-pairs for specific langs:
" " au Filetype clojure,scheme,lisp,fennel let b:AutoPairs = {}
" " au Filetype clojure,scheme,lisp,fennel let b:AutoPairs = {'(':')', '[':']', '{':'}', '"':'"'}
" " au Filetype clojure,scheme,lisp,fennel let b:AutoPairs = {'(':')', '[':']', '{':'}'}

" ------------------------------------------------------------
" PLUGIN: vim-sexp

let g:sexp_filetypes = 'clojure,scheme,lisp,fennel,janet,carp'
let g:sexp_enable_insert_mode_mappings = 0

" disable default mappings for indent (see vim-iced):
let g:sexp_mappings = {'sexp_indent': '', 'sexp_indent_top': ''}

" emulate text object for pair of elements
" i.e. key/value binding/expr test/expr
" from https://github.com/nbardiuk/dotfiles/blob/fc61451baa1df5d03d2e59c1dd3b5151915b760d/nix/.config/nixpkgs/home/init.fnl#L528-L546

autocmd Filetype clojure,scheme,lisp,fennel,janet,carp call SexpAdditions()
function SexpAdditions()
  " pair forward
  xmap <buffer> ip <Plug>(sexp_inner_element)<Plug>(sexp_move_to_next_element_tail)
  omap <buffer> ip <Cmd>normal vip<CR>

  " pair backward
  xmap <buffer> iP <Plug>(sexp_inner_element)o<Plug>(sexp_move_to_prev_element_head)
  omap <buffer> iP <Cmd>normal viP<CR>

  " swap pair
  nmap <buffer> >p vip>eo<Esc>
  nmap <buffer> <p vip<eo<Esc>

  xmap <buffer> >e <Plug>(sexp_swap_element_forward)
  xmap <buffer> <e <Plug>(sexp_swap_element_backward)
  xmap <buffer> >f <Plug>(sexp_swap_list_forward)
  xmap <buffer> <f <Plug>(sexp_swap_list_backward))
endfunction

  " Def a let (by Leaf Garland: https://clojurians.slack.com/archives/C0DF8R51A/p1652876174549129?thread_ts=1652858081.522919&cid=C0DF8R51A)
  nmap <localleader>cdl vie<m-e>y:IcedEval (def <c-r>")<cr>

" ------------------------------------------------------------
" PLUGIN: rainbow

let g:rainbow_active = 0 "set to 0 if you want to enable it later via :RainbowToggle

" ------------------------------------------------------------
" PLUGIN: vim-better-comments


" ------------------------------------------------------------
" ADDONS: Clerk

function! ClerkShow()
  exe "w"
  exe "IcedEval (nextjournal.clerk/show! \"" . expand("%:p") . "\")"
endfunction

function! ClerkClearCache()
  exe "IcedEval (nextjournal.clerk/clear-cache!)"
endfunction

function! ClerkShowCleared()
  exe ClerkClearCache()
  exe ClerkShow()
endfunction

nmap <silent> <localleader>cs m`:execute ClerkShow()<CR><c-o>
nmap <silent> <localleader>cl :execute ClerkClearCache()<CR>
nmap <silent> <localleader>cc m`:execute ClerkShowCleared()<CR><c-o>
"
" ------------------------------------------------------------
" ADDONS: flow-storm

function! CriteriumRequire()
  exe "normal gg"
  sleep 100m
  exe "IcedEval (require '[criterium.core :as crt])"
endfunction

nmap <silent> <localleader>crr m`:execute CriteriumRequire()<CR><c-o>

" ------------------------------------------------------------
" ADDONS: flow-storm

function! FlowStormRequire()
  exe "IcedEval (require 'flow-storm.api)"
  " exe "IcedEval (require '[flow-storm.api :as fs-api])"
endfunction

function! FlowStormConnect()
  exe "normal gg"
  sleep 100m
  call FlowStormRequire()
  exe "IcedEval (flow-storm.api/local-connect)"
  " exe "IcedEval (fs-api/local-connect)"
endfunction

function! FlowStormStop()
  exe "IcedEval (flow-storm.api/stop)"
  " exe "IcedEval (fs-api/stop)"
endfunction

" nmap <silent> <localleader>fcr :execute FlowStormRequire()<CR>
nmap <silent> <localleader>fsc m`:execute FlowStormConnect()<CR><c-o>
nmap <silent> <localleader>fss m`:execute FlowStormStop()<CR>

" ------------------------------------------------------------
" ADDONS: conjure

let g:conjure#filetypes = ['scheme', 'lisp', 'racket', 'fennel', 'janet']
" To ensure mappings won’t override:
" let g:conjure#mapping#prefix = '<localleader>c'

" let g:conjure#log#hud#width = 1.0
" let g:conjure#log#hud#height = 0.3
" let g:conjure#log#hud#overlap_padding = 0.1
" let g:conjure#log#hud#anchor = "SE"
let g:conjure#log#hud#enabled = v:false
let g:conjure#mapping#log_toggle = "ss"  " like Iced
let g:conjure#mapping#log_reset_soft = "sl"  " like Iced

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" LUA TEST


" Tutorial: https://vonheikemen.github.io/devlog/tools/configuring-neovim-using-lua/

" Embedding Lua files:

" lua require('basic')

" Inline Lua (multiple lines):

" lua <<EOF
" print('hello from lua')
" EOF

" Inline Lua (single commands):

" lua print('this also works')


