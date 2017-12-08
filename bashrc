# .bashrc					-*- shell-script -*-
# 
# This only applies to *interactive* shells (ones reading from a
# terminal, or invoked with -i), as well as ones started by rshd or
# similar which ar *NOT* login shells (i.e., not started as "-bash" or
# with --login).
# 
# It makes sense to say '. ~/.bashrc' in your .profile (or
# .bash_{login,profile} or whatever), since you probably want all of
# this stuff to apply to login shells too.

# I used to do this:
# 
# export BASH_ENV=$HOME/.bashrc
# 
# which would cause it to apply to most other (noninteractive)
# subshells too.  Unfortunately, this turns out to be undesirable,
# because (amongst other things) it breaks /usr/bin/which (a shell
# script) by stopming PATH before 'type -p' gets a chance to look for
# the program you're whiching.
# 
# Just in case I ever figure out the proper fix, though, there's a
# section down below that only gets executed when we really are
# interactive, as opposed to having been read for some other reason
# (like BASH_ENV, as above.)

######################################################################
# Is this shell interactive?
function interactive () {
    # We should just be able to test $PS1, but the gdm maintainers
    # think that it's nice to invoke the .xsession from a login shell,
    # and the bash maintainers think it's nice to have login shells
    # set $PS1...  <sigh>
    case $- in
	*i*)	return 0; ;;							# True
	*)	return 1; ;;							# False
    esac									# (Thanks to "UNIX F.A.Q.", 1993)
}

######################################################################
# PATH setup (may be overridden by .site-bashrc):
OLD_PATH="$PATH"			# Not exported

######################################################################
# Local setup:
[ -r ~/.site-bashrc ] && . ~/.site-bashrc

######################################################################
# Final PATH setup: (may be overridden by .site-bashrc):
# Prefixes to default PATH:
[ -d ~/bin ] && PATH=~/bin:$PATH	# Add home directory to path
# Suffixes to default PATH:
for p in /usr/games/bin /usr/games ; do
    [[ -d "$p" ]] && PATH="$PATH":"$p"
done

######################################################################
# Environment setup:
umask 022

export PAGER=less

# Apparently Mac OS crontab doesn't use the shell to parse $VISUAL, so
# we use a work-around involving a script in ~/bin/:
if [[ -x $HOME/bin/emacs-nw ]] ; then
    export VISUAL=emacs-nw
else
    export VISUAL="emacs -nw"
fi
export EDITOR="$VISUAL"

#export PERL5LIB=~/lib/perl
#export LANG=en_CA
#export LC_COLLATE=C
export BLOCKSIZE=1024	# Make df / du behave reasonably

#export http_proxy=http://chsmc.bellglobal.com:80/
#export no_proxy=youngst.org,ruah.dyndns.org,localhost

# Program-specific:
#export PGPPATH=~/.pgp
#export IPOD_MOUNTPOINT="/mnt/ipod"

# For CVS:
#export CVS_RSH=ssh
#export CVSROOT=/var/local/cvsroot
#export CVSEDITOR=vi

# Go:
export GOPATH=$HOME/src/go
[ -d $GOPATH/bin ] && PATH=$PATH:$GOPATH/bin	# Add go (output) binaries

# Python (pyenv and virtualenv):
if [[ $(type -P pyenv) ]] ; then
    eval "$(pyenv init -)"
    eval "$(pyenv virtualenv-init -)"
fi

######################################################################
# Special code to handle xrlogin:
TERMparse="bin/TERMparse"
if [ "$TERM" -a -x $TERMparse ] && echo $TERM | grep -q %% ; then
    eval `$TERMparse`
fi

######################################################################
# Interactive-only setup.  Alias definitions go here:
if interactive ; then
    ########################################
    # Terminal-related setup:

    # Set a fancy prompt:
    EL='\['`tput el`'\]'
    SMSO='\['`tput smso`'\]'
    RMSO='\['`tput rmso`'\]'
    BOLD='\['`tput bold`'\]'
    SGR0='\['`tput sgr0`'\]'
    PS1="${EL}=> \$?\n${SMSO}[\u@\h]${RMSO} ${BOLD}(\#)${SGR0} \w\\\$ "
    #PS1="\h:\W \u\$"
    unset EL SMSO RMSO BOLD SGR0
    
    # check the window size after each command and, if necessary,
    # update the values of LINES and COLUMNS.
    shopt -s checkwinsize

    # Should be the default:
    # tty -s && mesg y

    ########################################
    # History, completion, etc:

    # Set CDPATH to something sensible; take into account that ~/ may
    # not be the same as ~$USER/, and that if we don't explicitly
    # include '.' as the first entry then things get a bit confusing.
    # 
    # Note that it's not a good idea to export this to non-interactive
    # shells.
    # 
    # Disabled because it breaks the (cd foo && tar cf - ) | ...idiom.
    #CDPATH=.:~:~$USER

    # Don't put duplicate lines in the history:
    HISTCONTROL=ignoredups

    # Enable programmable completion features:
    if [ -r /etc/bash_completion ]; then
	. /etc/bash_completion
    fi

    ########################################
    # Aliases and shell functions:

    # Do ls -FC when output is to a terminal; ls otherwise:
    function myls () { if tty -s 0>&1 ; then ls -FC "$@" ; else ls "$@" ; fi ; }
    alias ls=myls

    # Enable color support in ls:
    #eval `dircolors`
    #alias ls='ls --color=auto '

    alias rs='eval `resize`'
    #alias w3mb='w3m http://ruah.dyndns.org/~cpcallen/bookmarks.html'
    #function auplay () { sox -t au "$@" -t ub - | vplay ; }
    #alias dr 'emacs ~/doc/dreams'
    #function cdvplay () { cdda2wav -b 8 -r 22050 -m "$@" -  |bplay -s 22050 -b 8 ; }
    #alias csemacs='ssh -f lassar.uwaterloo.ca "(setenv CS452p yes ; tcsh -c emacs)"'
    #alias slashbored='echo -e "HEAD / HTTP/1.0\n\n" | nc slashdot.org 80 |grep "^X-" | grep -v "^X-Powered-By"'

    # For doing s/// on filenames (for move, copy, or symlink)
    function mvs () { E="$1" ; shift ; for f in "$@" ; do mv    "$f" "`echo "$f" | sed -Ee "$E"`" ; done ; }
    function cps () { E="$1" ; shift ; for f in "$@" ; do cp    "$f" "`echo "$f" | sed -Ee "$E"`" ; done ; }
    function lns () { E="$1" ; shift ; for f in "$@" ; do ln -s "$f" "`echo "$f" | sed -Ee "$E"`" ; done ; }

    # This needs generalising...
    function xauthdist () { xauth extract - :0 |ssh localhost -l cpcallen xauth -f .Xauthority merge - ; }

    # To set xterm titles:
    function xtermtitle () { echo -e "\e]0;$@\007" ; }

    # Fetch photos from android phone:
    alias get-a='get -vs A -S /Volumes/GPHONE\ 32GB'
    alias compare-a='compare -vS /Volumes/GPHONE\ 32GB'
    alias get-tmp='get -vW $HOME/Pictures/src-tmp'
fi

