#####################################################################
#
#  .zlogin file
#  2023-10-26
#
#  Read in after the .zshrc file when you log in.
#  Not read in for subsequent shells.  For setting up
#  terminal and global environment characteristics.
#
#  Reference:
#  http://www.gentei.org/~yuuji/support/zsh/files/zlogin
#
#####################################################################

# Global but interactive-use only variables
manpath=(/usr/*/man(N-/) /usr/local/*/man(N-/) /var/*/man(N-/))

export MANPATH
#export LESS='-iscj5'
#export LESS='eMqc'
export LESS='-R'
export JLESSCHARSET=japanese
export LESSCHARSET=utf-8
#export BLOCKSIZE=k

# man
# bat can be used as a colorizing pager for man, by setting the MANPAGER environment variable:
#export MANPAGER="sh -c 'col -bx | bat -l man -p'"
#man 2 select

# editor
export EDITOR=vim

###.zlogin file ends here
