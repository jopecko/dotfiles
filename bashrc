# .bashrc
#
# This file is read (normally) by interactive shells only. It serves to define
# elements that are not inherited through the environment, such as aliases,
# functions, etc.
#

[ -f /etc/bashrc ] && . /etc/bashrc
[ -f $HOME/.java_profile ] && . $HOME/.java_profile
[ -f $HOME/.profile ] && . $HOME/.profile
[ -f $HOME/.env_vars ] && . $HOME/.env_vars # machine-dependent env vars

if [ -f ~/.env ]; then
  . ~/.env
fi

if [ -f ~/.aliases ]; then
  . ~/.aliases
fi

for file in $(ls ~/.bash); do
  . ~/.bash/$file
done

# add optional tools to the path
# http://github.com/guides/compiling-and-installing-git-on-mac-os-x
#for a in local $(ls /opt/ | grep -v local); do
#    FULLPATH=/opt/$a
#    if [ -x $FULLPATH ]; then
#        if [ -x $FULLPATH/bin ]; then
#            export PATH="$FULLPATH/bin:$PATH"
#        fi
#        if [ -x $FULLPATH/sbin ]; then
#            export PATH="$FULLPATH/sbin:$PATH"
#        fi
#        if [ -x $FULLPATH/share/aclocal ]; then
#            export ACLOCAL_FLAGS="-I $FULLPATH/share/aclocal $ACLOCAL_FLAGS"
#        fi
#        if [ -x $FULLPATH/man ]; then
#            export MANPATH="$FULLPATH/man:$MANPATH"
#        fi
#        if [ -x $FULLPATH/share/man ]; then
#            export MANPATH="$FULLPATH/share/man:$MANPATH"
#        fi
#        if [ -x $FULLPATH/lib/pkgconfig ]; then
#            export PKG_CONFIG_PATH="$FULLPATH/lib/pkgconfig/:$PKG_CONFIG_PATH"
#        fi
#    fi
#done

# Add some useful directories to our path
paths="."
paths="$paths $HOME/bin"
paths="$paths /usr/local/sbin"

# I commonly install utilities in $HOME/opt; find any
# bin directories therein and add them to paths
for binDir in `find $HOME/opt -type d -name bin`; do
    paths="$paths $binDir"
done

for path in $paths; do
    # make sure this entry doesn't already exist in the PATH
    if `echo $PATH | egrep $path'(\:|$)' >/dev/null 2>&1`; then
	continue
    fi
    if [ -d $path ]; then
	newpath=$newpath:$path
    fi
done

PATH=`echo $PATH | sed -e 's/^\://' -e 's/\:/:/g'`

# our path takes precedence over the one defined by the
# parent process; therefore place the original at the end
PATH="$newpath:$PATH"
export PATH

# http://help.github.com/working-with-key-passphrases/
SSH_ENV="$HOME/.ssh/environment"

function start_agent {
  echo "Initializing new SSH agent..."
  /usr/bin/ssh-agent | sed 's/^echo/#echo/' > "${SSH_ENV}"
  echo succeeded
  chmod 600 "${SSH_ENV}"
  . "${SSH_ENV}" > /dev/null
  /usr/bin/ssh-add;
}

# Source SSH settings, if applicable
#if [ -f "${SSH_ENV}" ]; then
#  . "${SSH_ENV}" > /dev/null
#  #ps ${SSH_AGENT_PID} doesn't work under cywgin
#  ps -ef | grep ${SSH_AGENT_PID} | grep ssh-agent$ > /dev/null || {
#    start_agent;
#  }
#else
#  start_agent;
#fi

