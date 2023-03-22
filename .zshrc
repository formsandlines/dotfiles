# private aliases
source ~/.dotfiles-private/aliases.sh

alias typora="open -a typora"
alias nvini="nvim ~/.dotfiles/.config/nvim/init.vim"
alias jc="~/j903-user/temp/launch.command ; exit;"
alias swank="ros run -e '(ql:quickload :swank) (swank:create-server)'"
alias reee="exec "$SHELL # restarts shell env
alias reeel="exec "$SHELL" -l" # restarts shell env, re-sources .zprofile
alias zc="nvim ~/.dotfiles/.zshrc"
alias zp="nvim ~/.dotfiles/.zprofile"
alias ice="iced repl"
alias icep="iced repl app"
alias pwdc="pwd | pbcopy" # copy current path
alias temperature="sudo powermetrics --samplers smc |grep -i 'CPU die temperature'" # monitor temperature of MacBook Pro
alias tem8="otherjava temurin8"
alias aichat="(cd ~/alpaca.cpp/ ; rlwrap ./chat)"
alias chicken-home="csi -R chicken.platform -p '(chicken-home)'"
# alias tetris="bb -Sdeps '{:deps {io.github.borkdude/console-tetris {:git/sha "2d3bee34ea93c84608c7cc5994ae70480b2df54c"}}}' -m tetris.core"

fuzz() { file=$(fzf) && nvim "$file"; }
# fuzz() {
#   file=$(fzf)
#   if [ ${#file} -gt 0 ]; then
#     nvim "$file"
#   else
#     :
#   fi
# }

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
  else
    jpath=$TEMURIN_17_HOME # default
  fi

  export JAVA_HOME=$jpath
  export PATH=$JAVA_HOME"/bin:"$PATH
}

# serendipity
echo "Docstring of the day (script by @borkdude):"
~/.dotfiles/scripts/random_doc.clj
