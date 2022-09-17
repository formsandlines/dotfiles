# whatever this is
if command -v pyenv 1>/dev/null 2>&1; then
  eval "$(pyenv init -)"
fi
if [ -e ~/.nix-profile/etc/profile.d/nix.sh ]; then . ~/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

# aliases
source ~/.dotfiles-private/aliases.sh
alias typora="open -a typora"
alias nvini="nvim ~/.dotfiles/.config/nvim/init.vim"
alias jc="~/j903-user/temp/launch.command ; exit;" # ???
alias swank="ros run -e '(ql:quickload :swank) (swank:create-server)'"
alias reee="exec '$SHELL'" # restarts shell env
alias zc="nvim ~/.dotfiles/.zshrc"
alias pwdc="pwd | pbcopy" # copy current path
alias temperature="sudo powermetrics --samplers smc |grep -i 'CPU die temperature'" # monitor temperature of MacBook Pro
alias javapath="export PATH=$JAVA_HOME/bin:$PATH"

# https://github.com/nvbn/thefuck
eval $(thefuck --alias) # corrects previous command


# JAVA version madness
# Note: rename (in <jvm-path>/Contents/) 'Info.plist.disabled' back to 'Info.plist'
#       to re-enable graalvm Java versions
#       see https://stackoverflow.com/questions/21964709/how-to-set-or-change-the-default-java-jdk-version-on-macos
export JVMPATH="/Library/Java/JavaVirtualMachines"
export GRAALVM_17_HOME="$JVMPATH/graalvm-ce-java17-22.0.0.2/Contents/Home"
export GRAALVM_GLUON_17_HOME="$JVMPATH/graalvm-svm-java17-darwin-gluon-22.0.0.3-Final/Contents/Home"
export TEMURIN_8_HOME="$JVMPATH/temurin-8.jdk/Contents/Home"
export TEMURIN_11_HOME="$JVMPATH/temurin-11.jdk/Contents/Home"
export TEMURIN_17_HOME="$JVMPATH/temurin-17.jdk/Contents/Home"
export TEMURIN_18_HOME="$JVMPATH/temurin-18.jdk/Contents/Home"
export OPENJDK_11_HOME="$JVMPATH/openjdk-11.jdk/Contents/Home"
export OPENJDK_17_HOME="$JVMPATH/openjdk-17.jdk/Contents/Home"
export OPENJDK_18_HOME="$JVMPATH/openjdk.jdk/Contents/Home"

# switch Java version with these commands (need to add to $PATH afterwards, use javapath alias):
alias graal17="export JAVA_HOME=$GRAALVM_17_HOME"
alias gluon17="export JAVA_HOME=$GRAALVM_GLUON_17_HOME"
alias openjdk11="export JAVA_HOME=$OPENJDK_11_HOME"
alias openjdk17="export JAVA_HOME=$OPENJDK_17_HOME"
alias openjdk18="export JAVA_HOME=$OPENJDK_18_HOME"
alias temurin8="export JAVA_HOME=$TEMURIN_8_HOME"
alias temurin11="export JAVA_HOME=$TEMURIN_11_HOME"
alias temurin17="export JAVA_HOME=$TEMURIN_17_HOME"
alias temurin18="export JAVA_HOME=$TEMURIN_18_HOME"
# default Java:
temurin17

# Guile paths (for Scheme)
export GUILE_LOAD_PATH="/usr/local/share/guile/site/3.0"
export GUILE_LOAD_COMPILED_PATH="/usr/local/lib/guile/3.0/site-ccache"
export GUILE_SYSTEM_EXTENSIONS_PATH="/usr/local/lib/guile/3.0/extensions"

# PATH
export PATH=~/.emacs.d/bin:$PATH
export PATH=$JAVA_HOME/bin:$PATH
# export PATH=$PATH:~/.config/nvim/plugged/vim-iced/bin
export PATH=$PATH:~/.local/share/nvim/plugged/vim-iced/bin


# Opam configuration (for OCaml)
test -r ~/.opam/opam-init/init.zsh && . ~/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

if type rg &> /dev/null; then
  export FZF_DEFAULT_COMMAND='rg --files'
  # export FZF_DEFAULT_COMMAND='rg --files --follow --no-ignore-vcs --hidden -g "!{node_modules/*,.git/*}"'
  export FZF_DEFAULT_OPTS='-m --height 50% --border'
fi
