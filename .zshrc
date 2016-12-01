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

#fetch
#neofetch --image ~/Pictures/wp3.jpg
neofetch
    
#colorz
/Users/eccentricayman/.config/base16-shell/scripts/base16-brewer.sh

alias gvim="open -a macvim"
alias lock="/Applications/Lock"
alias emacss="emacsclient"
    
function gitshit() {
    git add --all && git commit -m "$1" && git push
}

#get rid of annoying autocorrect
unsetopt correct

#export EDITOR=/usr/local/Cellar/emacs/HEAD-cbb2e84/bin/emacs
PATH="/Users/eccentricayman/perl5/bin${PATH:+:${PATH}}"; export PATH;
PERL5LIB="/Users/eccentricayman/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="/Users/eccentricayman/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"/Users/eccentricayman/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=/Users/eccentricayman/perl5"; export PERL_MM_OPT;
