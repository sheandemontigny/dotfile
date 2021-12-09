# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
source ~/.profile
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/shean/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall
source ~/powerlevel10k/powerlevel10k.zsh-theme

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

# Plugins 
source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
source ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# Custom config
bindkey "^[[1;5C" forward-word
bindkey "^[[1;5D" backward-word

alias vi="nvim"
alias vim="nvim"
alias ls="ls --color=auto"
alias ll="ls -la"
alias l.="ls -d .*"
alias startx="startx ~/.xinitrc"
alias config="git --git-dir=$HOME/.cfg/ --work-tree=$HOME"
alias ssh-root-cwh="TERM=xterm-256color ssh root@monitoring.softdb.com -p 2052"

export PATH=~/.emacs.d/bin:$PATH
export PATH=~/scripts:$PATH

export VISUAL=vim
export EDITOR="$VISUAL"

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/home/shean/google-cloud-sdk/path.zsh.inc' ]; then . '/home/shean/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/home/shean/google-cloud-sdk/completion.zsh.inc' ]; then . '/home/shean/google-cloud-sdk/completion.zsh.inc'; fi


export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
