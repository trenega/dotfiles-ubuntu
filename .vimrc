"-----------------------------------------------------------------------------
"
" File name     :  .vimrc --- on Ubuntu
" Copyright     :  nis:trenega@github.com
" 2023-10-27
"
"-----------------------------------------------------------------------------

set nocompatible

" vim-plugin Settigs-------------------------------
call plug#begin()
" The default plugin directory will be as follows:
"   - Vim (Linux/macOS): '~/.vim/plugged'
"   - Vim (Windows): '~/vimfiles/plugged'
"   - Neovim (Linux/macOS/Windows): stdpath('data') . '/plugged'
" You can specify a custom plugin directory by passing it as the argument
"   - eniijimatakashi993@gmail.com. `call plug#begin('~/.vim/plugged')`
"   - Avoid using standard Vim directory names like 'plugin'
"
" Make sure you use single quotes

" WRITE PLUGINS HERE!-----------------------------

" Mark change on vim
Plug 'airblade/vim-gitgutter'

" Git on vim
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'

" Disply indent line
Plug 'Yggdroot/indentLine'

" translate.vim
Plug 'skanehira/translate.vim'

" deepl.vim
Plug 'ryicoh/deepl.vim'

" fern.vim (filer)
Plug 'lambdalisue/fern.vim'
Plug 'lambdalisue/nerdfont.vim'
Plug 'lambdalisue/fern-renderer-nerdfont.vim'
Plug 'lambdalisue/glyph-palette.vim'
Plug 'lambdalisue/fern-git-status.vim'

" winresizer
" Very simple vim plugin for easy resizing of your vim windows.
Plug 'simeji/winresizer'

" minibufexpl.vim
Plug 'fholgado/minibufexpl.vim'

" Solarized install
Plug 'altercation/vim-colors-solarized'

" iceberg.vim
Plug 'cocopon/iceberg.vim'

" 日本語ヘルプをインストールする
Plug 'vim-jp/vimdoc-ja'

" goyo.vim Setting
Plug 'junegunn/goyo.vim'

" limelight.vim
Plug 'junegunn/limelight.vim'

" vim-sweep_trail
Plug 'vim-jp/vim-sweep_trail'

" fzf.vim
Plug 'junegunn/fzf.vim'

" eskk.vim
Plug 'vim-skk/eskk.vim'

" neoterm
Plug 'kassio/neoterm'

" Auto close parentheses and repeat by dot dot dot...
Plug 'cohama/lexima.vim'

" vim-expand-region
Plug 'terryma/vim-expand-region'

" tpope/vim-commentary
Plug 'tpope/vim-commentary'


" END WRITE PLUGINS HERE!---------------------------

" " Initialize plugin system
" - Automatically executes `filetype plugin indent on` and `syntax enable`.
call plug#end()
" You can revert the settings after the call like so:
"   filetype indent off   " Disable file-type-specific indentation
"   syntax off            " Disable syntax highlighting
" End vim-plugin Settigs----------------------------


" ----------------------------------------------------------------------------
" PLUGINS SETTINGS
" ----------------------------------------------------------------------------

" vim-expand-region
vmap v <Plug>(expand_region_expand)
vmap <C-v> <Plug>(expand_region_shrink)

"goyo + limelight (focus mode)------------
nnoremap <silent> <space>gy :Goyo<CR>
let g:goyo_width = 120
nnoremap <silent> <space>ll :Limelight!!<CR>
let g:limelight_default_coefficient = 0.7
 let g:limelight_paragraph_span = 1
autocmd! User GoyoEnter Limelight
autocmd! User GoyoLeave Limelight!

"End goyo + limelight (focus mode)--------

"deepl.vim--------------------------------
" フリー版のエンドポイントを指定
let g:deepl#endpoint = "https://api-free.deepl.com/v2/translate"

" AuthKey
let g:deepl#auth_key = "027ec6a8-c7a9-627c-40d6-2a6b3824b210:fx"

" replace a visual selection
vmap ,e <Cmd>call deepl#v("EN")<CR>
vmap ,j <Cmd>call deepl#v("JA")<CR>

" translate a current line and display on a new line
nmap ,e yypV<Cmd>call deepl#v("EN")<CR>
nmap ,j yypV<Cmd>call deepl#v("JA")<CR>

"End deepl.vim----------------------------

"minibufexpl.vim--------------------------
map <silent><Space>b :MBEOpen<cr>
map <silent><Space>bc :MBEClose<cr>
map <silent><Space>bt :MBEToggle<cr>

nnoremap <silent> bn :<C-u>:bnext<CR>
nnoremap <silent> b1 :<C-u>:b1<CR>
nnoremap <silent> b2 :<C-u>:b2<CR>
nnoremap <silent> b3 :<C-u>:b3<CR>
nnoremap <silent> b4 :<C-u>:b4<CR>
nnoremap <silent> b5 :<C-u>:b5<CR>
nnoremap <silent> b6 :<C-u>:b6<CR>
nnoremap <silent> b7 :<C-u>:b7<CR>
nnoremap <silent> b8 :<C-u>:b8<CR>
nnoremap <silent> b9 :<C-u>:b9<CR>

let g:miniBufExplMapWindowNavVim = 1
let g:miniBufExplMapWindowNavArrows = 1

"refer: https://uskey.hatenablog.com/entry/2015/08/16/080000
" 無条件でバッファ一覧が開く
let g:miniBufExplorerMoreThanOne = 0

" MiniBufExplorer 内を'hjkl'で移動
let g:miniBufExplMapWindowNavVim = 1

"End minibufexpl.vim----------------------

"fern.vim (filer)-------------------------
nnoremap <silent><Space>f :Fern . -reveal=%<CR>
let g:fern#default_hidden=1

  function! FernInit() abort
    nmap <buffer> v <Plug>(fern-action-open:side)
    nmap <buffer> M <Plug>(fern-action-new-dir)
    nmap <buffer> ! <Plug>(fern-action-hidden:toggle)
    nmap <buffer> - <Plug>(fern-action-mark:toggle)
    vmap <buffer> - <Plug>(fern-action-mark:toggle)
    nmap <buffer> C <Plug>(fern-action-clipboard-copy)
    nmap <buffer> X <Plug>(fern-action-clipboard-move)
    nmap <buffer> P <Plug>(fern-action-clipboard-paste)
    nmap <buffer> h <Plug>(fern-action-collapse)
    nmap <buffer> c <Plug>(fern-action-copy)
    nmap <buffer> <leader>h <Plug>(fern-action-leave)
    nmap <buffer> m <Plug>(fern-action-move)
    nmap <buffer> N <Plug>(fern-action-new-file)
    nmap <buffer> <cr> <Plug>(fern-action-open-or-enter)
    nmap <buffer> l <Plug>(fern-action-open-or-expand)
    nmap <buffer> s <Plug>(fern-action-open:select)
    nmap <buffer> t <Plug>(fern-action-open:tabedit)
    nmap <buffer> <C-l> <Plug>(fern-action-reload)
    nmap <buffer> r <Plug>(fern-action-rename)
    nmap <buffer> i <Plug>(fern-action-reveal)
    nmap <buffer> D <Plug>(fern-action-trash)
    nmap <buffer> y <Plug>(fern-action-yank)
    nmap <buffer> gr <Plug>(fern-action-grep)
    nmap <buffer> d <Plug>(fern-action-remove)
    nmap <buffer> B <Plug>(fern-action-save-as-bookmark)
    nmap <buffer> cd <Plug>(fern-action-tcd)
    nmap <buffer> <C-h> <C-w>h
    nmap <buffer> <C-l> <C-w>l
  endfunction
  augroup FernEvents
    autocmd!
    autocmd FileType fern call FernInit()
  augroup END

  let g:fern#disable_default_mappings = 1

" icon
" fern-renderer-nerdfont.vim
  let g:fern#renderer = 'nerdfont'
  let g:fern#renderer#nerdfont#indent_markers = 1

" icon
" glyph-palette.vim
  augroup my-glyph-palette
    autocmd! *
    autocmd FileType fern call glyph_palette#apply()
    autocmd FileType nerdtree,startify call glyph_palette#apply()
  augroup END

"End fern.vim (filer)---------------------

" ----------------------------------------------------------------------------
" OPTIONS
" ----------------------------------------------------------------------------

syntax enable                                   " Essential for syntax

filetype plugin indent on                       " Essential for filetype plugins.

set autoindent                                  " Carry over indenting from previous line

set clipboard=unnamedplus

set encoding=utf-8                              " UTF-8 by default

scriptencoding utf-8

set expandtab                                   " Change Tab'code to Space'code

set fileencodings=ucs-boms,utf-8,euc-jp,cp932

set fileformats=unix,dos,mac                    " Prefer Unix

set helplang=ja,en                              " help language

set history=5000                                " How many lines of history to save

set hlsearch                                    " Hiligth searching

set ignorecase                                  " Case insensitive

set incsearch                                   " Search as you type

set infercase                                   " ignorecase optionの副作用を解除する(cf.Practical Vim, Drew Neil p.354)

set mouse=a                                     " Move cursor by mouse

set nocp incsearch

set relativenumber                              " relative number

set smartcase                                   " 大文字/小文字の区別を予測してくれる

set smartindent

set shiftwidth=4                                " Number of spaces to shift for autoindent or >,<

set showmatch                                   " Hilight matching braces/parens/etc.

set softtabstop=4                               " Spaces 'feel' like tabs

set tabstop=4                                   " The One True Tab

set wildmenu                                    " Show possible completions on command line

set wildmode=full                               " List all options and complete

set nobackup                                    " Does not make backup file

set backspace=indent,eol,start                  " lexima option

set signcolumn=yes                              " Show gitgutter column always

set termguicolors                               " True Color

set cursorline                                  " cursorline on

set laststatus=2                                " Disply statuslines 2

set statusline=%<%f\ %h%m%r%{FugitiveStatusline()}%=%-14.(%l,%c%V%)\ %P
                                                " Disply statusline [Git(master)]
                                                " ERROR
                                                " 'FugitivesStatusline()'

"インサートモードから出ずにVimを使いこなす--------------------
" Reference: https://woodyzootopia.github.io/2019/11/インサートモードから出ずにVimを使いこなす
" cnoremap mode: command line
" inoremap mode: insert

" 左へ移動
cnoremap <C-b> <Left>
inoremap <C-b> <Left>

" 右へ移動
cnoremap <C-f> <Right>
inoremap <C-f> <Right>

" 上へ移動
cnoremap <C-p> <Up>
inoremap <C-p> <Up>

" 下へ移動
cnoremap <C-n> <Down>
inoremap <C-n> <Down>

" 行頭へ移動
cnoremap <C-a> <Home>
inoremap <C-a> <Home>

" 行末へ移動
cnoremap <C-e> <End>
inoremap <C-e> <End>

" 一文字削除
cnoremap <C-d> <Del>
inoremap <C-d> <Del>

"End インサートモードから出ずにVimを使いこなす---------------

" ハイライトを消去する
nnoremap <silent> <C-l> :<C-u>nohlsearch<CR><C-l>

" 挿入モードでのDelete, Backspace
inoremap <C-d> <Del>
imap <C-h> <BS>

" CTRL + ] で右にエスケープする
inoremap <C-]> <Esc><Right>

"-----------------------------------------------------------------------
"LEADER KEY MAPPINGS
"-----------------------------------------------------------------------

" Vimの生産性を高める12の方法
" How to boost your Vim productivity (2014-03-21) by Adam Stankiewicz
" Reference: https://postd.cc/how-to-boost-your-vim-productivity/
" let mapleader = "\<Space>"
let mapleader = ","

" Make a simple "search" text object.
" refer:https://vim.fandom.com/wiki/Copy_or_change_search_hit
" 検索を実行するには通常と同様に /something を使う
" cgn を押し、最初の一致項目を置換して、 <Esc> を押す
" n.n.n.n.n. と押して、全ての一致項目を確認しながら置換する
vnoremap <silent> s //e<C-r>=&selection=='exclusive'?'+1':''<CR><CR>
    \:<C-u>call histdel('search',-1)<Bar>let @/=histget('search',-1)<CR>gv
omap s :normal vs<CR>

" スペースキーを prefix にする例
" スペースキー単体では何も起きないようにする
" これをしておかないと、うっかり <Space> + 割り当ててないキーを
" 押すと <Space> の元の機能が発動する
" Reference: https://thinca.hatenablog.com/entry/q-as-prefix-key-in-vim
nnoremap <Leader> <Nop>

" {motion} のテキストを大文字にする。
" refer: ~/.config/nvim/dein/.cache/init.vim/.dein/doc/change.jax
map! <Leader>u <Esc>gUiw`]a

" カレントファイルを開く。
" これは Vim 外部でカレントファ イルに変更が加えられたとき、
" 開き直すのに便利である。
nnoremap <Leader>e :e<Space>

" 無名のバッファの編集を新規に開始する
nnoremap <Leader>n :enew<CR>

" ファイルを保存する
" refer: thinca vimrc
nnoremap <silent> <Leader>w :<C-u>update<CR>
nnoremap <silent> <Leader>W :<C-u>update!<CR>
" nvimを終了する
nnoremap <silent> <Leader>q :<C-u>quit<CR>
nnoremap <silent> <Leader>Q :<C-u>quit!<CR>

" ファイルを挿入する
nnoremap <Leader>r :r<Space>

" {count} なしの場合、カレントウィンドウを閉じる。
" もし {count} が与えられた場合、{count} ウィンドウを閉じる。
nnoremap <Leader>cl :clo

" <Space>p と <Space>y でシステムのクリップボードにコピー＆ペーストする
nmap <Leader><Leader> V

" Reference: https://postd.cc/how-to-boost-your-vim-productivity/
vmap <Leader>y "+y
vmap <Leader>d "+d
nmap <Leader>p "+p
nmap <Leader>P "+P
vmap <Leader>p "+p
vmap <Leader>P "+P

" 現在のウィンドウを水平に分割する
nnoremap <Leader>s :split<CR>

" 現在のウィンドウを垂直に分割する
nnoremap <Leader>v :vsplit<CR>

" 新しいタブページを開く
nnoremap <Leader>te :tabedit

" ウィンドウを閉じる
nnoremap <silent> <Leader>q :<C-u>quit<CR>

" move to current Window
" 上のWindowへ移動する
nnoremap <Leader>k <C-w>k

" 下のWindowへ移動する
nnoremap <Leader>j <C-w>j

" 左のWindowへ移動する
nnoremap <Leader>h <C-w>h

" 右のWindowへ移動する
nnoremap <Leader>l <C-w>l

" Hot key for open init.vim file
nnoremap <Leader>. :<C-u>edit $MYVIMRC<CR>

" Check for marks
nnoremap <Leader>mm :<C-u>marks<CR>

" Check for registers
nnoremap <Leader>re :<C-u>registers<CR>

" sweep_trail.vim
nnoremap <Leader>sw :<C-u>SweepTrail<CR>

" undotree
" The undo history visualizer for VIM
nnoremap <Leader>ut :<C-u>UndotreeToggle<CR>

"vim-fugitive----------------------------
" Reference: https://code-log.hatenablog.com/entry/2018/12/08/101732
nnoremap <Leader>ga :Git add %:p<CR><CR>
nnoremap <Leader>gc :Gcommit<CR><CR>
nnoremap <Leader>gs :Gstatus<CR>
nnoremap <Leader>gd :Gdiff<CR>
nnoremap <Leader>gl :GLlog<CR>
nnoremap <Leader>gb :Gblame<CR>
nnoremap <Leader>gb :Gblame<CR>

"End vim-fugitive-------------------------

"-----------------------------------------------------------------------
"End LEADER kEY MAPPINGS
"-----------------------------------------------------------------------

" :helpを3倍の速度で引く
nnoremap ,h :<C-u>help<Space>

" 検索後にジャンプした際に検索単語を画面中央に持ってくる
nnoremap n nzz
nnoremap N Nzz
nnoremap * *zz
nnoremap # #zz
nnoremap g* g*zz
nnoremap g# g#zz

" 'verymagic'
nnoremap / /\v

"Insert Mode Keymaps----------------------
" Change INSERT mode to NORMAL mode
inoremap <silent> fd <Esc>

" File Save
inoremap <silent> js <C-o>:write<CR>

" Scroll to center line
inoremap <silent> zz <C-o>zz

" Scroll to top line
inoremap <silent> zk <C-o>z<CR>

" Scroll to bottom line
inoremap <silent> zj <C-o>z-

"End Insert Mode Keymaps------------------

" ペースト設定 クリップボードからペーストする時だけ、インデントしない
if &term =~ "xterm"
        let &t_SI        .= "\e[?2004h"
        let &t_EI        .= "\e[?2004l"
        let &pastetoggle =  "\e[201~"

        function XTermPasteBegin(ret)
                set paste
                return a:ret
        endfunction

        inoremap <special> <expr> <Esc>[200~ XTermPasteBegin("")
endif

" 最後のカーソル位置を復元する--------------------------------
if has("autocmd")
        autocmd BufReadPost *
        \ if line("'\"") > 0 && line ("'\"") <= line("$") |
        \   exe "normal! g'\"" |
        \ endif
endif

"cursor Change----------------------------
" Reference: https://qiita.com/Linda_pp/items/9e0c94eb82b18071db34
if has('vim_starting')
    " 挿入モード時に非点滅の縦棒タイプのカーソル
    let &t_SI .= "\e[6 q"

    " ノーマルモード時に点滅のブロックタイプのカーソル
    let &t_EI .= "\e[1 q"

    " 置換モード時に非点滅の下線タイプのカーソル
    let &t_SR .= "\e[4 q"
    endif

" NOMAL modeのカーソルを非点滅させる
" Reference: https://chanko.hatenadiary.jp/entry/2016/10/28/162648
" let &t_EI .= "\e[2 q"

"カーソル下の単語をGoogleで検索する -------------------------
function! s:search_by_google()
    let line = line(".")
    let col  = col(".")
    let searchWord = expand("<cword>")
    if searchWord  != ''
        execute 'read !open https://www.google.co.jp/search\?q\=' . searchWord
        execute 'call cursor(' . line . ',' . col . ')'
    endif
endfunction
command! SearchByGoogle call s:search_by_google()
nnoremap <silent> <Space>gg :SearchByGoogle<CR>

"Vimの生産性を高める12の方法--------------
" How to boost your Vim productivity (2014-03-21) by Adam Stankiewicz
" Reference: https://postd.cc/how-to-boost-your-vim-productivity/
" 12<Enter> を押して、12行目に移動する（ 12G だと手首が曲がってしまう）
" Enterを押して、ファイルの末尾に移動する
nnoremap <CR> <Nop>
nnoremap <CR> G

"End Vimの生産性を高める12の方法----------

"cpと打つと ペーストモードになる
" Reference: https://kekaku.addisteria.com/wp/20170621231629
nnoremap cp :set paste<CR>

"挿入モードを抜けるとき、set nopaste を実行する。
autocmd InsertLeave * set nopaste

" ----------------------------------------------------------------------------
" ABBREVIATION
" ----------------------------------------------------------------------------

iabbrev .b #!/bin/bash
iabbrev .r #!/usr/bin/ruby
iabbrev EC # -*- coding: utf-8 -*-
iabbrev .e niijimatakashi993@icloud.com
iabbrev .g niijimatakashi993@gmail.com
iabbrev .t takashiniijima213@gmail.com
iabbrev .y takashiniijima213@yahoo.co.jp

" ----------------------------------------------------------------------------
" End ABBREVIATION
" ----------------------------------------------------------------------------

"-----------------------------------------------------------------------------
" 日付挿入
"-----------------------------------------------------------------------------
inoremap <Leader>date <C-R>=strftime('%Y/%m/%d')<CR>
inoremap <Leader>time <C-R>=strftime('%H:%M')<CR>
inoremap <Leader>w3cd <C-R>=strftime('%Y-%m-%dT%H:%M:%S+09:00')<CR>

"iceberg.vim
colorscheme iceberg

" 貼り付け時にペーストバッファが上書きされないようにする
" Reference: https://postd.cc/how-to-boost-your-vim-productivity/
" このコードを ~/.vimrc の末尾付近に置きます
" vp doesn't replace paste buffer
function! RestoreRegister()
  let @" = s:restore_reg
  return ''
endfunction
function! s:Repl()
  let s:restore_reg = @"
  return "p@=RestoreRegister()\<cr>"
endfunction
vmap <silent> <expr> p <sid>Repl()

""" .vimrc ends here
