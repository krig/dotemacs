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
setopt automenu # don't auto-list first match
setopt extendedglob
setopt listpacked
setopt listtypes
setopt completeinword          # not just at the end
setopt alwaystoend             # when complete from middle, move cursor
setopt histverify              # when using ! cmds, confirm first

# misc
unsetopt beep # don't beep
unsetopt notify # don't report background jobs immediately
unsetopt hup # don't set HUP to jobs when exiting

bindkey -e bold

# expand no matter where in the line we are
bindkey '	' expand-or-complete-prefix

if [ -e $HOME/.agent ]; then
    source $HOME/.agent
fi
for file in $HOME/.zsh/??-*; do
    source $file
done

export PS1='%(?..?:%? )%(!.%{[31m%}.%{[34m%})%B%n%b%{[00m%}.%{[34m%}%m%{[00m%} %~ %# '
export RPROMPT='%{[32m%}$GITBRANCH%{[00m%}%T'

# The following lines were added by compinstall
zstyle :compinstall filename '/home/krig/.zshrc'

autoload -Uz compinit
compinit

# End of lines added by compinstall
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/cache
zstyle ':completion:*' glob 1
#zstyle ':completion:*' menu select=long-list select=0
#zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
zstyle ':completion:*' verbose yes
zstyle ':completion:*:messages' format '%d'
zstyle ':completion:*' completer _expand _force_rehash _complete _approximate _ignored
zstyle ':completion:*:default' list-prompt '%S%M matches%s'
zstyle ':completion:*:default' menu 'select=0'

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
zstyle ':completion:*:descriptions' format '%B%d%b'
# ignore files already given for rm, kill, diff
zstyle ':completion:*:(rm|kill|diff):*' ignore-line yes

# Completing process IDs with menu selection:
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:kill:*'   force-list always

# supposed to speed things up
zstyle ':completion:*' accept-exact '*(N)'

_force_rehash() {
  (( CURRENT == 1 )) && rehash
  return 1  # Because we didn't really complete anything
}

# URL encode something and print it.
function url-encode; {
        setopt extendedglob
        echo "${${(j: :)@}//(#b)(?)/%$[[##16]##${match[1]}]}"
}

# Search google for the given keywords.
function google; {
        $VIEW "http://www.google.com/search?q=`url-encode \"${(j: :)@}\"`"
}

