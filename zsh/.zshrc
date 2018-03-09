# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

#loads theme
#autoload -Uz promptinit
#promptinit
prompt sorin

#colorz
#source /Users/eccentricayman/.config/base16-shell/scripts/base16-brewer.sh

#fetch
fortune -s | cowsay | lolcat -t
#neofetch

#alias gvim="open -a macvim"
alias ec="emacsclient"

function gitshit() {
    git add --all && git commit -m "$1" && git push
}

#fzf command
export FZF_DEFAULT_COMMAND='ag --hidden --ignore .git --ignore "Library/*" --ignore "Music/*" --ignore "Pictures/*" --depth 5 -g ""'

#find file and open it in emacs
function fe() {
    emacs -nw $(fzf) 2>& /dev/null
}

#get rid of annoying autocorrect
unsetopt correct

export EDITOR=/usr/local/bin/emacs
export ANDROID_HOME=~/Library/Android/sdk

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

PATH="/Users/eccentricayman/perl5/bin${PATH:+:${PATH}}:/Users/eccentricayman/Github/pokemon:/Users/eccentricayman/Library/Android/sdk/tools:/Users/eccentricayman/Library/Android/sdk/platform-tools:/Users/eccentricayman/Library/Android/sdk/tools/bin:/Library/TeX/texbin:; export PATH;"
