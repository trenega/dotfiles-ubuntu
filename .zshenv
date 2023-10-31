#################################################################### 
#
#  .zshenv file
#
#  initial setup file for both interactive and noninteractive zsh
#
#  2023-10-26
#
#################################################################### 

#Sample .zshenv file----------------------
# http://www.gentei.org/~yuuji/support/zsh/files/zshenv
# zshの本 HIROSE Yuuji p.34

# Core file for max size
limit coredumpsize 0

# Setup command search path
typeset -U path

# (N-/) を付けることでことで存在しなければ無視してくれる
Path=($path /usr/*/bin(N-/) /usr/local/*/bin(N-/) /var/*/bin(N-/))

#End Sample .zshenv file------------------

# Homebrew
eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"

# UDEVGothic_NF font path
PATH=$PATH":~/.local/share/fonts"

#--------------------------------------------------------------------
# PATH MODIFICATIONS
# -------------------------------------------------------------------

# Functions which modify the path given a directory, but only if the directory
# exists and is not already in the path. (Super useful in ~/.zshlocal)
_append_to_path() {
  if [ -d $1 -a -z ${path[(r)$1]} ]; then
    path=($1 $path);
  fi
}

### .zshenv ends here
