# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH=/Users/tuomo.virolainen/.oh-my-zsh
export PYENV_ROOT="$HOME/.pyenv"
export OH_MY_ZSH="$HOME/.oh-my-zsh"
export PYTHON_BUILD_MIRROR_URL="http://yyuu.github.io/pythons/"

export JAVAFX_HOME="/Users/tuomo.virolainen/javafx-sdk-11.0.2/lib"

export OCAML_TOPLEVEL_PATH="/home/tuomo.virolainen/.opam/system/lib/toplevel"

export FZF_DEFAULT_COMMAND='ag --hidden --ignore .git -g ""'

# For compilers to find zlib you may need to set:
export LDFLAGS="${LDFLAGS} -L/usr/local/opt/zlib/lib"
export CPPFLAGS="${CPPFLAGS} -I/usr/local/opt/zlib/include"

# For pkg-config to find zlib you may need to set:
export PKG_CONFIG_PATH="${PKG_CONFIG_PATH} /usr/local/opt/zlib/lib/pkgconfig"

eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

# mappings for Ctrl-left-arrow and Ctrl-right-arrow for word moving
# Without these mappings, they will not work in Kitty/Zsh
bindkey -e
bindkey "^[[1;3C" forward-word
bindkey "^[[1;3D" backward-word
# bindkey "\e\e[1;3C" forward-word
# bindkey "[1;3D" backward-word
# bindkey "\e\e^[1;3C" forward-word
# bindkey "^[1;3D" backward-word
# bindkey "\e\e3C" forward-word
# bindkey "3D" backward-word
# bindkey "\e[1;5C": forward-word
# bindkey "\e[1;5D": backward-word
# bindkey "\e[5C": forward-word
# bindkey "\e[5D": backward-word
# bindkey "\e[1;3C": forward-word
# bindkey "\e[1;3D": backward-word
# bindkey "\e3C": forward-word
# bindkey "\e3D": backward-word
# bindkey "\e\e[C": forward-word
# bindkey "\e\e[D": backward-word

autoload -Uz zkbd
bindkey -v

[[ -n "${key[C-Left]}" ]]  && bindkey "${key[C-Left]}"  backward-word
[[ -n "${key[C-Right]}" ]] && bindkey "${key[C-Right]}" forward-word

unset LSCOLORS
export CLICOLOR=1
# export CLICOLOR_FORCE=1

# set 256 color profile where possible
# if [[ $COLORTERM == gnome-* && $TERM == xterm ]] && infocmp gnome-256color >/dev/null 2>&1; then
#     export TERM=gnome-256color
# elif infocmp xterm-256color >/dev/null 2>&1; then
#     export TERM=xterm-256color
# fi

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="powerlevel10k/powerlevel10k"

DEFAULT_USER=$USER

# Set list of themes to load
# Setting this variable when ZSH_THEME=random
# cause zsh load theme from this variable instead of
# looking in ~/.oh-my-zsh/themes/
# An empty array have no effect
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
  zsh-autosuggestions
  git
)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/rsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

alias v='nvim'
alias vi='nvim'
alias vim='nvim'
source /Users/tuomo.virolainen/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

alias rsbcl='rlwrap sbcl'

alias t='tig'

alias cat='bat'

alias grep='rg'

alias rmcontainers='docker container rm $(docker container stop $(docker container ls -aq))'
alias rmimages='docker image rm $(docker image ls -aq)'

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

. /Users/tuomo.virolainen/bin/z.sh

alias l='exa -la --header --git'
alias ls='exa'
alias c='clear'
alias e='exit'

ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=6'

# OPAM configuration
# . /Users/tuomo.virolainen/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

eval `opam config env`

export PATH="$HOME/.jenv/bin:$PATH"
eval "$(jenv init -)"

# Whenever a command is executed, write it to a global history
# PROMPT_COMMAND="history -a ~/.zsh_history.global; $PROMPT_COMMAND"

# On C-r set HISTFILE and run hh
# bindkey "C-r" "HISTFILE=~/.bash_history.global hh"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# Source chtf
if [[ -f /usr/local/share/chtf/chtf.sh ]]; then
    source "/usr/local/share/chtf/chtf.sh"
fi

eval $(thefuck --alias)


export LC_ALL=en_US.UTF-8

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="/Users/tuomo.virolainen/.sdkman"
[[ -s "/Users/tuomo.virolainen/.sdkman/bin/sdkman-init.sh" ]] && source "/Users/tuomo.virolainen/.sdkman/bin/sdkman-init.sh"

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/Users/tuomo.virolainen/anaconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/Users/tuomo.virolainen/anaconda3/etc/profile.d/conda.sh" ]; then
        . "/Users/tuomo.virolainen/anaconda3/etc/profile.d/conda.sh"
    else
        export PATH="/Users/tuomo.virolainen/anaconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

