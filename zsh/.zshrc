
# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

#path
export EDITOR=/Applications/Emacs.app/Contents/MacOS/Emacs
export ANDROID_HOME=~/Library/Android/sdk
PATH="/Users/eccentricayman/perl5/bin${PATH:+:${PATH}}:/usr/local/bin:/Users/eccentricayman/Github/pokemon:/Users/eccentricayman/Library/Android/sdk/tools:/Users/eccentricayman/Library/Android/sdk/platform-tools:/Users/eccentricayman/Library/Android/sdk/tools/bin:/Library/TeX/texbin; export PATH;"
export PATH="/usr/local/opt/llvm/bin:$PATH"
export PATH="/Users/eccentricayman/Library/Python/3.7/bin:$PATH"
export PATH=/opt/homebrew/bin:$PATH
export INDRA_HOME="Users/eccentricayman/Github/IndraABM"

#theme settings
SPACESHIP_PROMPT_ADD_NEWLINE=false
SPACESHIP_PROMPT_SEPARATE_LINE=false
SPACESHIP_CHAR_SUFFIX=" "
SPACESHIP_CHAR_SYMBOL=❯
SPACESHIP_CHAR_COLOR_SUCCESS=magenta
SPACESHIP_DIR_COLOR=blue
SPACESHIP_GIT_BRANCH_COLOR=green
SPACESHIP_GIT_STATUS_COLOR=green
SPACESHIP_GIT_STATUS_PREFIX=""
SPACESHIP_GIT_STATUS_SUFFIX=""
SPACESHIP_GIT_STATUS_UNTRACKED=" ◼"
SPACESHIP_GIT_STATUS_ADDED=" ✚"
SPACESHIP_GIT_STATUS_MODIFIED=" ✱"
SPACESHIP_GIT_STATUS_RENAMED=" »"
SPACESHIP_GIT_STATUS_DELETED=" ✖"
SPACESHIP_GIT_STATUS_STASHED=" ✭"
SPACESHIP_GIT_STATUS_UNMERGED=" ="
SPACESHIP_GIT_STATUS_AHEAD=" ⬆"
SPACESHIP_GIT_STATUS_BEHIND=" ⬇"
SPACESHIP_GIT_STATUS_DIVERGED=" ⇕"
SPACESHIP_PACKAGE_SHOW=false
SPACESHIP_NODE_SHOW=false
SPACESHIP_RUBY_SHOW=false
SPACESHIP_ELIXIR_SHOW=false
SPACESHIP_XCODE_SHOW_LOCAL=false
SPACESHIP_XCODE_SHOW_GLOBAL=false
SPACESHIP_SWIFT_SHOW_LOCAL=false
SPACESHIP_SWIFT_SHOW_GLOBAL=false
SPACESHIP_GOLANG_SHOW=false
SPACESHIP_PHP_SHOW=false
SPACESHIP_RUST_SHOW=false
SPACESHIP_HASKELL_SHOW=false
SPACESHIP_JULIA_SHOW=false
SPACESHIP_CONDA_SHOW=false
SPACESHIP_DOTNET_SHOW=false
SPACESHIP_EMBER_SHOW=false
SPACESHIP_KUBECONTEXT_SHOW=false
SPACESHIP_BATTERY_SHOW=false

#colorz
#source /Users/eccentricayman/.config/base16-shell/scripts/base16-brewer.sh

#fetch
fortune -s | cowsay | lolcat -t
#neofetch

#alias gvim="open -a macvim"
alias ec="emacsclient"
alias emacs="/Applications/Emacs.app/Contents/MacOS/Emacs"
alias python="python3"

function gitshit() {
    git add --all && git commit -m "$1" && git push
}

#fzf command
export FZF_DEFAULT_COMMAND='ag --hidden --ignore .git --ignore "Library/*" --ignore "Music/*" --ignore "Pictures/*" --depth 5 -g ""'

#find file and open it in emacs
function emacsf() {
    emacs $(fzf) 2>& /dev/null
}

#cd directory fuzzy
cdf() {
    local file
	file="$(fzf +m -q "$*")"
	cd "$(dirname "$file")" || return
}

#get rid of annoying autocorrect
unsetopt correct

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

export PATH="/opt/homebrew/sbin:$PATH"
export PATH="/opt/homebrew/opt/php@8.0/bin:$PATH"
