# .bash_profile
#
# Put anything special that should happen *only* for login shells here

[ -f ~/.bash_aliases ] && source ~/.bash_aliases
[ -f ~/.bash_functions ] && source ~/.bash_functions
[ -f ~/.bash_completions ] && source ~/.bash_completions

if [ -f $HOME/.bashrc ]; then
	. $HOME/.bashrc
fi

