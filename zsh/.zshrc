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
source /Users/eccentricayman/.config/base16-shell/scripts/base16-google-light.sh

#fetch
neofetch --image ~/Pictures/boo2.png
#neofetch

alias gvim="open -a macvim"
alias emacss="emacsclient"

function gitshit() {
    git add --all && git commit -m "$1" && git push
}

function fe() {
    emacsclient $(fzf) 2>& /dev/null
}

#get rid of annoying autocorrect
unsetopt correct

export EDITOR=/usr/local/Cellar/emacs/HEAD-3ec1503/bin/emacs
PATH="/Users/eccentricayman/perl5/bin${PATH:+:${PATH}}"; export PATH;
