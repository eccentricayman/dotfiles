#
# Executes commands at the start of an interactive session.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

#loads theme
autoload -Uz promptinit
promptinit
prompt sorin

#colorz
source /Users/eccentricayman/.config/base16-shell/scripts/base16-brewer.sh

#fetch
fortune -as | cowsay -f moose | lolcat -t
#neofetch

alias gvim="open -a macvim"
alias emacss="emacsclient"

function please() {
    echo -e "Okay, but only because you asked nicely."
    sudo $(fc -ln -1)
}

function gitshit() {
    git add --all && git commit -m "$1" && git push
}

function fe() {
    emacsclient $(fzf) 2>& /dev/null
}

#get rid of annoying autocorrect
unsetopt correct

export EDITOR=/usr/local/Cellar/emacs/HEAD-3ec1503/bin/emacs
export ANDROID_HOME=~/Library/Android/sdk

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

PATH="/Users/eccentricayman/perl5/bin${PATH:+:${PATH}}:/Users/eccentricayman/Github/pokemon:/Users/eccentricayman/Library/Android/sdk/tools:/Users/eccentricayman/Library/Android/sdk/platform-tools:/Users/eccentricayman/Library/Android/sdk/tools/bin:/Library/TeX/texbin:; export PATH;"

