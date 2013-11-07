# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

umask 022

read_path() {
        while read dir; do
                [ -d $dir ] && PATH=$PATH:$dir
        done
        unset dir
        export PATH
}

read_manpath() {
        while read mandir; do
                [ -d $mandir ] && MANPATH=$MANPATH:$mandir
        done
        unset mandir
        export MANPATH
}

# add other dirs to PATH, including user's private bin
read_path dir << !
$HOME/bin
!
### if [ -z "${LD_LIBRARY_PATH}" ]
### then
###     export LD_LIBRARY_PATH=${HOME}/lib
### else
###     export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${HOME}/lib
### fi

#-----------------------
# Environment variables
#-----------------------
if [ -z "${PYTHONPATH}" ]
then
    export PYTHONPATH=${HOME}/Python:${HOME}/src/qlib/trunk/code/utils
else
    export PYTHONPATH=${PYTHONPATH}:${HOME}/Python
fi

if [ -d "${HOME}/src/qlib/trunk/code/utils" ]
then
    export PATH=${PATH}:${HOME}/src/qlib/trunk/code/utils
fi

if [ -z "${HOSTNAME}" ]
then
    export HOSTNAME=`uname -n`
fi

# set up printers
LPDEST=???
LPCOLOR=???

http_proxy=http://alpext11.proxy.corporate.ge.com:80
https_proxy=http://alpext11.proxy.corporate.ge.com:80
#EDITOR="emacs -nw"
LESS='-rFM'
PAGER='less -rFM'
export http_proxy https_proxy EDITOR LESS PAGER

if [ `uname -s``uname -m` == "Linuxx86_64" ]; then
    OSVER=Linux64
    export OSVER
else echo "OS = `uname -s`, architecture = `uname -m`"
fi


shopt -s checkwinsize # check window size, update LINES/COLUMNS after each cmd
shopt -s extglob      # extended pattern matching in path names
shopt -s histappend   # append to the history file, don't overwrite it

set -o emacs

# HISTORY options
HISTCONTROL=ignoreboth # don't store duplicate lines or lines with leading space
HISTSIZE=2000          # number of commands
HISTFILESIZE=2000      # number of lines

# make less more friendly for non-text input files, see lesspipe(1)
#[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
if [ "$TERM" = "emacs" ]; then
    force_color_prompt=no
else
    force_color_prompt=yes
fi

if [ -n "$force_color_prompt" ]; then
    if [ "$TERM" = "cygwin" ]; then
        color_prompt=yes
    elif [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
        # We have color support; assume it's compliant with Ecma-48
        # (ISO/IEC-6429). (Lack of such support is extremely rare, and such
        # a case would tend to support setf rather than setaf.)
        color_prompt=yes
    else
        color_prompt=
    fi
fi
unset force_color_prompt

# Attribute codes:
# 00=none 01=bold 04=underscore 05=blink 07=reverse 08=concealed
# Text color codes:
# 30=black 31=red 32=green 33=yellow 34=blue 35=magenta 36=cyan 37=white
# Background color codes:
# 40=black 41=red 42=green 43=yellow 44=blue 45=magenta 46=cyan 47=white

# PS1='[0;35m${PWD}[0m $ '
# PS1='[0m\@ ${HOSTNAME}:[0;04m${PWD}[0m $ ' # in color
if [ "$color_prompt" = yes ]; then
#   PS1='\t \[\033[01;31m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
#   PS1='\t \[\033[00;32m\]\w\[\033[00m\]\$ '
#   PS1='\t \[\033[01;33m\]\w\[\033[00m\]> '
    PS1='\t \[\033[01;32m\]\w>\[\033[00m\] '
else
#   PS1='\t \u@\h:\w\$ '
    PS1='\t \w\$ '
fi

export PS1

#-----------------------------------
# Color Support & Alias definitions
#-----------------------------------
if [ "$color_prompt" = yes -a -x /usr/bin/dircolors ]; then
    test -r ~/.dir-colors && eval "$(dircolors -b ~/.dir-colors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    alias dir='dir --color=auto'
    alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'

    DIR_COLOR='0;35m'
    GREP_COLOR="43;30"
    export DIR_COLOR GREP_COLOR
fi

# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

alias R='R -q'
alias eamcs='emacs --no-splash'
alias grpe="grep"
alias l='less'
alias la='ls -A'
alias ll='ls -l'
alias scr="echo [0m" # make screen back to normal
alias xeamcs='xemacs'
alias xs='cd'

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

# You may want to put all your functions into a separate file like
# ~/.bash_functions, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.
if [ -f ~/.bash_functions ]; then
    . ~/.bash_functions
fi
