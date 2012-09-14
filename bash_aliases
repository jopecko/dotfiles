# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dir\
colors -b)"
    alias ls='ls --color=auto'
    alias dir='dir --color=auto'
    alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# some more ls aliases
alias ll='ls -alFh'
alias la='ls -A'
alias l='ls -CF'

alias emac="emacs -nw -q --no-site-file"
alias ri="ri -f ansi -T"

#alias grep="grep --color=auto"
alias grepp="ps aux | grep"

# ff '*.[ch]'
alias ff='find . -follow -name'

alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'
alias mkdir='mkdir -p'

# just plain lazy
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'

# hunt the disk hog
alias duck='du -cks * | sort -rn | head -11'

# clear out history
alias hclear='history -c; clear'

alias ga='git add'
alias gb='git branch'
alias gc='git commit'
alias gd='git diff'
alias go='git checkout'

