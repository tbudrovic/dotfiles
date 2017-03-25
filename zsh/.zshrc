export TERM="rxvt-unicode-256color"

# key bindings and zle settings
bindkey -v
bindkey -M viins '^r' history-incremental-search-backward
bindkey -M vicmd '^r' history-incremental-search-backward

# history settings
setopt histignorealldups sharehistory
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.zsh_history

# autocompletion settings
autoload -Uz compinit
compinit

zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' menu select=2
eval "$(dircolors -b)"
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=* l:|=*'
zstyle ':completion:*' menu select=long
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl false
zstyle ':completion:*' verbose true

zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,tty,cputime,cmd'

# pager settings
export PAGER=less
export LESS="-FXR"

# colorize ls output
eval "$(dircolors -b)"
alias ls='ls -F --color=auto'

# enviroment
export EDITOR="emacsclient -c"
export PATH=$PATH:~/bin:~/.gems/bin:~/node_modules/.bin
export GEM_HOME=~/.gems

# powerlevel9k
POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(context dir ssh vcs)
POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(status root_indicator background_jobs dir_writable)
POWERLEVEL9K_PROMPT_ON_NEWLINE=true
POWERLEVEL9K_VI_INSERT_MODE_STRING="I"
POWERLEVEL9K_VI_COMMAND_MODE_STRING="C"
source ~/dotfiles/powerlevel9k/powerlevel9k.zsh-theme

/usr/games/fortune
