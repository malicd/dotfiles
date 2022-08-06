set expandtab
set tabstop=2
set softtabstop=2
set shiftwidth=2

" This is necessary for VimTeX to load properly. The "indent" is optional.
" Note that most plugin managers will do this automatically.
filetype plugin indent on

" This enables Vim's and neovim's syntax-related features. Without this, some
" VimTeX features will not work (see ":help vimtex-requirements" for more
" info).
syntax enable

" Viewer options: One may configure the viewer either by specifying a built-in
" viewer method:
let g:vimtex_view_method = 'zathura'

set spell spelllang=en_us
let g:tex_flavor='latex'
set conceallevel=1
let g:tex_conceal='abdmg'
