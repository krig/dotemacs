HISTFILE=~/.zsh/historyfile
HISTSIZE=5000
SAVEHIST=5000
# History handling
setopt appendhistory  #append to history
setopt inc_append_history # append immediately
setopt hist_expire_dups_first # expire duplicates in history first
setopt hist_ignore_dups # don't add dupes to history
setopt share_history

# interactive configuration
setopt nomatch # print error if no match is found
setopt correct # correct spelling mistakes in commands
setopt autocd # change to directory without "cd"
setopt auto_list # auto-list all completion choices when hitting TAB
# automenu is needed for menu-based selection
setopt auto_menu # don't auto-list first match
unsetopt extendedglob

# misc
unsetopt beep # don't beep
unsetopt notify # don't report background jobs immediately
unsetopt hup # don't set HUP to jobs when exiting

bindkey -e bold
#bindkey -v # vim bindings
# Ctrl+R history searching with vim bindings
#bindkey -M viins '' history-incremental-search-backward
#bindkey -M vicmd '' history-incremental-search-backward
# Ctrl+ A/E even with vim bindings
#bindkey -M viins '' beginning-of-line
#bindkey -M vicmd '' beginning-of-line
#bindkey -M viins '' end-of-line
#bindkey -M vicmd '' end-of-line
#bindkey -M viins '' forward-word
#bindkey -M vicmd '' forward-word

# expand no matter where in the line we are
bindkey '	' expand-or-complete-prefix

if [ -e $HOME/.agent ]; then
    source $HOME/.agent
fi
for file in $HOME/.zsh/scripts/*; do
    source $file
done
# username@host:directory (git branch)>
# red if root, git branch in green
#export PS1=':: %(!.%{[31m%}.%{[36m%})%n@%m:%{[00m%}%~%{[32m%}$GITBRANCH%{[00m%}> '

export PS1='%(?..?:%? )%(!.%{[31m%}.%{[36m%})%n:%{[00m%}%~%# '
export RPROMPT='%{[32m%}$GITBRANCH%{[00m%}%T'

# The following lines were added by compinstall
zstyle :compinstall filename '/home/krig/.zshrc'

autoload -Uz compinit
compinit

# End of lines added by compinstall
zstyle ':completion:*' glob 1
zstyle ':completion:*' menu select=long-list select=0
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

# host autocompletion for hosts from known_hosts
zstyle -e ':completion::*:hosts' hosts 'reply=(
	${=${${(f)"$(cat {/etc/ssh_,~/.ssh/known_}hosts(|2)(N) \
      /dev/null)"}%%[# ]*}//,/ }
    ${${${(M)${(s:# :)${(zj:# :)${(Lf)"$([[ -f ~/.ssh/config ]] && < \
      ~/.ssh/config)"}%%\#*}}##host(|name) *}#host(|name) }/\*}
	)'

# set $PATH when calling sudo
zstyle ':completion:*:sudo:*' command-path /usr/local/sbin /usr/local/bin \
                             /usr/sbin /usr/bin /sbin /bin
zstyle ':completion:*:functions' ignored-patterns '_*' # Ignore completion functions for commands you don't have

# ignore files already given for rm, kill, diff
zstyle ':completion:*:(rm|kill|diff):*' ignore-line yes

if [ "$HOST" = "emu" ]; then
    mpx
fi
