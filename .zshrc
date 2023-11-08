####################################################################
#
#  .zshrc file
#
#  initial setup file for only interactive  zsh
#
#  2023-10-26
#
####################################################################

# Setting from ubuntu .bashrc------------
# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# colored GCC warnings and errors
#export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# End Setting from ubuntu .bashrc---------

##Sample .zshrc file----------------------
# Reference: http://www.gentei.org/~yuuji/support/zsh/files/zshrc
#SET SHELL VARIABLE-----------------------
# WORDCHARS=$WORDCHARS:s,/,,
#HISTSIZE=200 HISTFILE=~/.zhistory SAVEHIST=180

#SET SHELL OPTIONS------------------------
# 有効にしてあるのは副作用の少ないもの
setopt auto_cd auto_pushd auto_remove_slash auto_name_dirs
setopt extended_history hist_ignore_dups hist_ignore_space prompt_subst
setopt extended_glob list_types no_beep always_last_prompt
setopt cdable_vars sh_word_split auto_param_keys pushd_ignore_dups
setopt correct
setopt PUSHD_IGNORE_DUPS
# 便利だが副作用の強いものはコメントアウト
#setopt auto_menu  correct rm_star_silent sun_keyboard_hack inc_append_history
setopt share_history hist_reduce_blanks hist_ignore_all_dups

#END SET SHELL OPTIONS--------------------

#colordiff
# refs: https://qiita.com/catatsuy/items/8bafef2a60762a1c9f0f
if [[ -x `which colordiff` ]]; then
  alias diff='colordiff -u'
else
  alias diff='diff -u'
fi

export LESS='-R'

##ALIAS AND FUNCTIONS----------------------
# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

alias mv='mv -i' alias cp='cp -bi'
alias copy='cp -ip' move='mv -i'
alias rm='trash-put'
alias ln='ln -b'

alias d='cd ~/dotfiles-ubuntu'
alias tes='cd ~/pl/test'
alias ..='cd ..'
alias ..2='cd ../..'
alias ..3='cd ../../..'

# alias diff='diff -y --suppress-common-lines --color=aut

alias vino='vim -u NONE -N'

alias em='emacs'                                # emacs
alias emd='emacs --debug-init'

#alias grep='ggrep -E --color=auto'              # ggrep, color options
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

# ghc
# alias ghci='stack ghci'
# alias ghc='stack ghc --'
# alias runghc='stack runghc --'

# Git
alias g='git'
alias gA='git add --all :/'
alias ga='git add'
alias gb='git branch'
alias gc='git commit'
alias gd='git diff'
alias gl='git log --oneline --all'
alias glo='git lol'
alias gls='git log --show-signature'
alias gm='gitmoji -c'
alias gmt='git mergetool --tool=nvimdiff2'
alias gp='git push origin master'
alias gpo='git push origin'
alias gs='git status'
alias gsw='git switch'

alias gal='alias | grep git'

# irb simple prompt
alias irbs='irb --simple-prompt'
alias ruby='ruby -w'
alias ru='ruby -w'
alias gcc='gcc -Wall -fno-pic -fomit-frame-pointer'

# install-data
alias install-ls='vi ~/data/install/install-data'

# Common Lisp: PEPL
alias lispi='rlwrap ros run'

# Clojure repl
alias clj='lein repl'

#End ALIAS AND FUNCTIONS------------------

# viins keymap
bindkey -v

# emacs keymap
# bindkey -e
bindkey '^p'  history-beginning-search-backward
bindkey '^n'  history-beginning-search-forward

# Use <C-q> push-line (zsh emacs keymap)
stty start undef

#補完システムを利用:----------------------
# 補完の挙動が分かりやすくなる2つの設定のみを記述
zstyle ':completion:*' format '%BCompleting %d%b'
zstyle ':completion:*' group-name ''
autoload -U compinit && compinit

##End Sample .zshrc file-------------------

##BINDING KEYS-----------------------------
# viins (like emacs-mode)
# refs: https://qiita.com/b4b4r07/items/8db0257d2e6f6b19ecb9
bindkey -M viins '\er' history-incremental-pattern-search-forward
bindkey -M viins '^?'  backward-delete-char
bindkey -M viins '^A'  beginning-of-line
bindkey -M viins '^B'  backward-char
bindkey -M viins '^D'  delete-char-or-list
bindkey -M viins '^E'  end-of-line
bindkey -M viins '^F'  forward-char
bindkey -M viins '^G'  send-break
bindkey -M viins '^H'  backward-delete-char
bindkey -M viins '^K'  kill-line
bindkey -M viins '^N'  down-line-or-history
bindkey -M viins '^P'  up-line-or-history
bindkey -M viins '^R'  history-incremental-pattern-search-backward
bindkey -M viins '^U'  backward-kill-line
# bindkey -M viins '^W'  backward-kill-word
bindkey -M viins '^Y'  yank

##End BINDING KEYS--------------------------

#Complement for git commands--------------
# Git git-completionでコマンドやブランチ名を補完する方法(zsh)
# Refs: https://alpacat.com/blog/git-completion-zsh/
# git-completion
fpath=(~/.zsh/completion $fpath)
autoload -U compinit
compinit -u

#End Complement for git commands----------

# -------------------------------------------------------------------
# APPLICATION CUSTOMIZATIONS
# -------------------------------------------------------------------

# INTERNAL UTILITY FUNCTIONS---------------
# Returns whether the given command is executable or aliased.
# Reference: VIM AFTER 15 YEARS （2017-10-17） by Ian Langworth
# .zshrc by Ian Langworth
# https://postd.cc/vim3/
_has() {
  return $( whence $1 >/dev/null )
}

# Returns whether out terminal supports color.
_color() {
  return $( [ -z "$INSIDE_EMACS" -a -z "$VIMRUNTIME" ] )
}

#End INTERNAL UTILITY FUNCTIONS---------------

# GNU grep
if _color; then
  export GREP_COLORS='mt=1;32'
fi

# Ack is better than grep
if ! _color; then
  alias ack='ack --nocolor'
fi

# GNU and BSD ls colorization.
if _color; then
  export LS_COLORS='no=00:fi=00:di=01;34:ln=01;36:pi=33:so=01;35:bd=33;01:cd=33;01:or=01;05;37;41:mi=01;37;41:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.gz=01;31:*.bz2=01;31:*.bz=01;31:*.tz=01;31:*.rpm=01;31:*.cpio=01;31:*.jpg=01;35:*.gif=01;35:*.bmp=01;35:*.xbm=01;35:*.xpm=01;35:*.png=01;35:*.tif=01;35:'
  export LSCOLORS='ExGxFxdxCxDxDxcxcxxCxc'
  export CLICOLOR=1
fi

# prompt pure----------------------------------------
# sindresorhus/pure
# Pretty, minimal and fast ZSH prompt
#fpath+=("$(brew --prefix)/share/zsh/site-functions")
fpath+=($HOME/.zsh/pure)

autoload -U promptinit; promptinit

# optionally define some options
# PURE_CMD_MAX_EXEC_TIME=10
#PURE_GIT_DOWN_ARROW
#PURE_GIT_UP_ARROW
#PURE_GIT_STASH_SYMBOL

# change the path color
zstyle :prompt:pure:path color cyan

# change the color for both `prompt:success` and `prompt:error`
zstyle ':prompt:pure:prompt:*' color cyan

# turn on git stash status
zstyle :prompt:pure:git:stash show yes
#zstyle :prompt:pure:git:fetch only_upstream yes

prompt pure

# End prompt pure------------------------------------

#fzfを活用してTerminalの作業効率を高める------
# fvim: ファイル名検索+Vimで開くファイルをカレントディレクトリからfzfで検索可能に
# refs: https://yiskw713.hatenablog.com/entry/2022/01/12/200000
# refs: https://momozo.tech/2021/03/10/fzf%E3%81%A7zsh%E3%82%BF%E3%83%BC%E3%83%9F%E3%83%8A%E3%83%AB%E4%BD%9C%E6%A5%AD%E3%82%92%E5%8A%B9%E7%8E%87%E5%8C%96/
# vim version
fvim() {
  local file
  file=$(
         rg --files --hidden --follow --glob "!**/.git/*" | fzf \
             --preview 'bat  --color=always --style=header,grid {}' --preview-window=right:60%
     )
  vim "$file"
}
alias fv="fvim"

# emacs version
femacs() {
  local file
  file=$(
         rg --files --hidden --follow --glob "!**/.git/*" | fzf \
             --preview 'bat  --color=always --style=header,grid {}' --preview-window=right:60%
     )
  emacs "$file"
}
alias fe="femacs"

#     ____      ____
#    / __/___  / __/
#   / /_/_  / / /_
#  / __/ / /_/ __/
# /_/   /___/_/ key-bindings.zsh
#
# - $FZF_TMUX_OPTS
# - $FZF_CTRL_T_COMMAND
# - $FZF_CTRL_T_OPTS
# - $FZF_CTRL_R_OPTS
# - $FZF_ALT_C_COMMAND
# - $FZF_ALT_C_OPTS

# Key bindings
# ------------

# The code at the top and the bottom of this file is the same as in completion.zsh.
# Refer to that file for explanation.
if 'zmodload' 'zsh/parameter' 2>'/dev/null' && (( ${+options} )); then
  __fzf_key_bindings_options="options=(${(j: :)${(kv)options[@]}})"
else
  () {
    __fzf_key_bindings_options="setopt"
    'local' '__fzf_opt'
    for __fzf_opt in "${(@)${(@f)$(set -o)}%% *}"; do
      if [[ -o "$__fzf_opt" ]]; then
        __fzf_key_bindings_options+=" -o $__fzf_opt"
      else
        __fzf_key_bindings_options+=" +o $__fzf_opt"
      fi
    done
  }
fi

'emulate' 'zsh' '-o' 'no_aliases'

{

[[ -o interactive ]] || return 0

# CTRL-T - Paste the selected file path(s) into the command line
__fsel() {
  local cmd="${FZF_CTRL_T_COMMAND:-"command find -L . -mindepth 1 \\( -path '*/\\.*' -o -fstype 'sysfs' -o -fstype 'devfs' -o -fstype 'devtmpfs' -o -fstype 'proc' \\) -prune \
    -o -type f -print \
    -o -type d -print \
    -o -type l -print 2> /dev/null | cut -b3-"}"
  setopt localoptions pipefail no_aliases 2> /dev/null
  local item
  eval "$cmd" | FZF_DEFAULT_OPTS="--height ${FZF_TMUX_HEIGHT:-40%} --reverse --bind=ctrl-z:ignore $FZF_DEFAULT_OPTS $FZF_CTRL_T_OPTS" $(__fzfcmd) -m "$@" | while read item; do
    echo -n "${(q)item} "
  done
  local ret=$?
  echo
  return $ret
}

__fzfcmd() {
  [ -n "$TMUX_PANE" ] && { [ "${FZF_TMUX:-0}" != 0 ] || [ -n "$FZF_TMUX_OPTS" ]; } &&
    echo "fzf-tmux ${FZF_TMUX_OPTS:--d${FZF_TMUX_HEIGHT:-40%}} -- " || echo "fzf"
}

fzf-file-widget() {
  LBUFFER="${LBUFFER}$(__fsel)"
  local ret=$?
  zle reset-prompt
  return $ret
}
zle     -N   fzf-file-widget
bindkey '^T' fzf-file-widget

# ALT-C - cd into the selected directory
fzf-cd-widget() {
  local cmd="${FZF_ALT_C_COMMAND:-"command find -L . -mindepth 1 \\( -path '*/\\.*' -o -fstype 'sysfs' -o -fstype 'devfs' -o -fstype 'devtmpfs' -o -fstype 'proc' \\) -prune \
    -o -type d -print 2> /dev/null | cut -b3-"}"
  setopt localoptions pipefail no_aliases 2> /dev/null
  local dir="$(eval "$cmd" | FZF_DEFAULT_OPTS="--height ${FZF_TMUX_HEIGHT:-40%} --reverse --bind=ctrl-z:ignore $FZF_DEFAULT_OPTS $FZF_ALT_C_OPTS" $(__fzfcmd) +m)"
  if [[ -z "$dir" ]]; then
    zle redisplay
    return 0
  fi
  zle push-line # Clear buffer. Auto-restored on next prompt.
  BUFFER="cd -- ${(q)dir}"
  zle accept-line
  local ret=$?
  unset dir # ensure this doesn't end up appearing in prompt expansion
  zle reset-prompt
  return $ret
}
zle     -N    fzf-cd-widget
bindkey '\ec' fzf-cd-widget

# CTRL-R - Paste the selected command from history into the command line
fzf-history-widget() {
  local selected num
  setopt localoptions noglobsubst noposixbuiltins pipefail no_aliases 2> /dev/null
  selected=( $(fc -rl 1 | perl -ne 'print if !$seen{(/^\s*[0-9]+\**\s+(.*)/, $1)}++' |
    FZF_DEFAULT_OPTS="--height ${FZF_TMUX_HEIGHT:-40%} $FZF_DEFAULT_OPTS -n2..,.. --tiebreak=index --bind=ctrl-r:toggle-sort,ctrl-z:ignore $FZF_CTRL_R_OPTS --query=${(qqq)LBUFFER} +m" $(__fzfcmd)) )
  local ret=$?
  if [ -n "$selected" ]; then
    num=$selected[1]
    if [ -n "$num" ]; then
      zle vi-fetch-history -n $num
    fi
  fi
  zle reset-prompt
  return $ret
}
zle     -N   fzf-history-widget
bindkey '^R' fzf-history-widget

} always {
  eval $__fzf_key_bindings_options
  'unset' '__fzf_key_bindings_options'
}

##End key-bindings.zsh--------------------

##Update fzf auto add code
# 設定用のfzf.zshを読み込む処理が追加されています
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}"/fzf/fzf.zsh ] && source "${XDG_CONFIG_HOME:-$HOME/.config}"/fzf/fzf.zsh

##Setup ssh-agent--------------------------
# refs: https://h2plus.biz/hiromitsu/entry/791
if [ -f ~/.ssh-agent ]; then
    . ~/.ssh-agent
fi
if [ -z "$SSH_AGENT_PID" ] || ! kill -0 $SSH_AGENT_PID; then
    ssh-agent > ~/.ssh-agent
    . ~/.ssh-agent
fi
ssh-add -l >& /dev/null || ssh-add

##End Setup ssh-agent----------------------

### .zshrc ends here
