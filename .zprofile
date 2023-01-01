# Set PATH, MANPATH, etc., for Homebrew.
eval "$(/opt/homebrew/bin/brew shellenv)"

if command -v pyenv 1>/dev/null 2>&1; then
  eval "$(pyenv init -)"
fi
# if [ -e ~/.nix-profile/etc/profile.d/nix.sh ]; then . ~/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

# Pyenv data storage location
export PYENV_ROOT=$HOME"/.pyenv"

# JAVA version madness
# Note: rename (in <jvm-path>/Contents/) 'Info.plist.disabled' back to 'Info.plist'
#       to re-enable graalvm Java versions
#       see https://stackoverflow.com/questions/21964709/how-to-set-or-change-the-default-java-jdk-version-on-macos
export JVMPATH="/Library/Java/JavaVirtualMachines"
export GRAALVM_17_HOME=$JVMPATH"/graalvm-ce-java17-22.0.0.2/Contents/Home"
export GRAALVM_GLUON_17_HOME=$JVMPATH"/graalvm-svm-java17-darwin-gluon-22.0.0.3-Final/Contents/Home"
export TEMURIN_8_HOME=$JVMPATH"/temurin-8.jdk/Contents/Home"
export TEMURIN_11_HOME=$JVMPATH"/temurin-11.jdk/Contents/Home"
export TEMURIN_17_HOME=$JVMPATH"/temurin-17.jdk/Contents/Home"
export TEMURIN_18_HOME=$JVMPATH"/temurin-18.jdk/Contents/Home"
export OPENJDK_11_HOME=$JVMPATH"/openjdk-11.jdk/Contents/Home"
export OPENJDK_17_HOME=$JVMPATH"/openjdk-17.jdk/Contents/Home"
export OPENJDK_18_HOME=$JVMPATH"/openjdk.jdk/Contents/Home"

export JAVA_HOME=$TEMURIN_17_HOME

# Guile paths (for Scheme)
export GUILE_LOAD_PATH="/usr/local/share/guile/site/3.0"
export GUILE_LOAD_COMPILED_PATH="/usr/local/lib/guile/3.0/site-ccache"
export GUILE_SYSTEM_EXTENSIONS_PATH="/usr/local/lib/guile/3.0/extensions"

# PATH
export PATH=$JAVA_HOME"/bin:"$PATH
export PATH="~/.local/share/nvim/plugged/vim-iced/bin:"$PATH
export PATH="/opt/homebrew/opt/luajit-openresty/bin:"$PATH
export PATH="~/.emacs.d/bin:"$PATH

# Opam configuration (for OCaml)
# test -r ~/.opam/opam-init/init.zsh && . ~/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true
# 
# if type rg &> /dev/null; then
#   export FZF_DEFAULT_COMMAND='rg --files'
#   # export FZF_DEFAULT_COMMAND='rg --files --follow --no-ignore-vcs --hidden -g "!{node_modules/*,.git/*}"'
#   export FZF_DEFAULT_OPTS='-m --height 50% --border'
# fi
# export PATH="$PATH:$HOME/.babashka/bbin/bin"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
