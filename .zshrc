# private aliases
source ~/.dotfiles-private/aliases.sh

alias typora="open -a typora"
alias nvini="(cd ~/.dotfiles/.config/nvim/ ; nvim init.vim)"
alias cljkondc="(cd ~/.dotfiles/.config/clj-kondo/ ; nvim config.edn)"
# alias jc="~/j903-user/temp/launch.command ; exit;"
alias jc="/Applications/j9.4/bin/jconsole"
alias janetrepl="janet -e '(import spork/netrepl) (netrepl/server)'"
alias swank="ros run -e '(ql:quickload :swank) (swank:create-server)'"
alias reee="exec "$SHELL # restarts shell env
alias reeel="exec "$SHELL" -l" # restarts shell env, re-sources .zprofile
alias zc="nvim ~/.dotfiles/.zshrc"
alias zp="nvim ~/.dotfiles/.zprofile"
alias ice="iced repl"
alias icedev="iced repl -A:dev"
alias icep="iced repl app"
alias pwdc="pwd | pbcopy" # copy current path
alias temperature="sudo powermetrics --samplers smc |grep -i 'CPU die temperature'" # monitor temperature of MacBook Pro
alias tem8="otherjava temurin8"
alias aichat="(cd ~/alpaca.cpp/ ; rlwrap ./chat)"
alias chicken-home="csi -R chicken.platform -p '(chicken-home)'"
export logoini="$HOME/.dotfiles/ucblogo-ini.lg"
alias ucblogo="/Applications/UCBLogo.app/Contents/MacOS/UCBLogo $logoini"
# alias tetris="bb -Sdeps '{:deps {io.github.borkdude/console-tetris {:git/sha "2d3bee34ea93c84608c7cc5994ae70480b2df54c"}}}' -m tetris.core"
alias usesc2="cs install scala:2.12.18 scalac:2.12.18"
alias usesc3="cs install scala scalac"
alias clemacs="bash -c 'exec emacs -nw -q -l ~/.emacs.d.learn/init.el'"
alias xemacs='open -a /Applications/Emacs.app'
alias demacs='open -n -a /Applications/Emacs.app --args --init-directory "~/.emacs.d.doom"'
# alias lemacs='open -n -a /Applications/Emacs.app --args --init-directory "~/.emacs.d.learn"'
# alias lemacs='open -n -a /Applications/Emacs.app --args --no-desktop -q -l ~/.emacs.d.learn/init.el'
alias emacs-debug="open -a /Applications/Emacs.app --args -q"
alias opem="open $1 -a /Applications/Emacs.app"
alias sgrep='run_sgrep'

function run_sgrep() {
  (cd ~/Dev/semantic-grep && ./sgrep "$@")
}

# emacs-open() {
#   path="$1"
#   open "$path" -a emacs
# }

# for Emacs vterm:
# see https://github.com/akermu/emacs-libvterm?tab=readme-ov-file#shell-side-configuration
vterm_printf() {
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ]); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}
# lets vterm know where the prompt ends (prompt tracking)
vterm_prompt_end() {
    vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
}
setopt PROMPT_SUBST
PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'
# lets vterm read and execute commands (message passing)
vterm_cmd() {
    local vterm_elisp
    vterm_elisp=""
    while [ $# -gt 0 ]; do
        vterm_elisp="$vterm_elisp""$(printf '"%s" ' "$(printf "%s" "$1" | sed -e 's|\\|\\\\|g' -e 's|"|\\"|g')")"
        shift
    done
    vterm_printf "51;E$vterm_elisp"
}
# functions to call some vterm commands
find_file() {
    vterm_cmd find-file "$(realpath "${@:-.}")"
}
open_file_below() {
    vterm_cmd find-file-below "$(realpath "${@:-.}")"
}
say() {
    vterm_cmd message "%s" "$*"
}


focusApp() {
  app="$1"
  osascript -e 'tell application "System Events" to set frontmost of process "'$app'" to true'
}

# fzf shell integration (according to setup instructions)
eval "$(fzf --zsh)"

fuzz() { file=$(fzf) && nvim "$file"; }
# fuzz() {
#   file=$(fzf)
#   if [ ${#file} -gt 0 ]; then
#     nvim "$file"
#   else
#     :
#   fi
# }

# add arg -s for strings
diffle() {
  if [[ $1 = "-s" ]]; then
    diff -u --color=always <(echo "$2") <(echo "$3") | less -r
  else
    diff -u --color=always "$1" "$2" | less -r
  fi
}

commonfiles() {
  a=$(ls -l "$1" | tail -n +2 | cut -f 9 -w)
  b=$(ls -l "$2" | tail -n +2 | cut -f 9 -w)

  grep -Ff <(echo "$a") <(echo "$b")
}
difffiles() {
  # output is in diff format (+/- and two extra lines with stats)
  a=$(ls "$1" | tr '\t' '\n')
  b=$(ls "$2" | tr '\t' '\n')

  diff -u <(echo "$a") <(echo "$b") | grep -E "^[+-]"
}

# function to switch Java version easily
otherjava() {
  if [[ $1 = "graal17" ]]; then
    jpath=$GRAALVM_17_HOME
  elif [[ $1 = "gluon17" ]]; then
    jpath=$GRAALVM_GLUON_17_HOME
  elif [[ $1 = "openjdk11" ]]; then
    jpath=$OPENJDK_11_HOME
  elif [[ $1 = "openjdk17" ]]; then
    jpath=$OPENJDK_17_HOME
  elif [[ $1 = "openjdk18" ]]; then
    jpath=$OPENJDK_18_HOME
  elif [[ $1 = "temurin8" ]]; then
    jpath=$TEMURIN_8_HOME
  elif [[ $1 = "temurin11" ]]; then
    jpath=$TEMURIN_11_HOME
  elif [[ $1 = "temurin17" ]]; then
    jpath=$TEMURIN_17_HOME
  elif [[ $1 = "temurin18" ]]; then
    jpath=$TEMURIN_18_HOME
  elif [[ $1 = "temurin20" ]]; then
    jpath=$TEMURIN_20_HOME
  else
    echo "Incorrect Java version!"
    jpath=$TEMURIN_17_HOME # default
  fi

  JAVA_HOME=$jpath
  PATH=$JAVA_HOME"/bin:"$PATH
}

# serendipity
# echo "Docstring of the day (script by @borkdude):"
# ~/.dotfiles/scripts/random_doc.clj
