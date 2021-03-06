#!/bin/bash
#
# Initial environment
# ~/.profile
#

# Change where configuration files go
# xdg
[ -z $XDG_CONFIG_HOME ] && export XDG_CONFIG_HOME=$HOME/.config
[ -z $XDG_CACHE_HOME ] && export XDG_CACHE_HOME=$HOME/.cache
[ -z $XDG_DATA_HOME ] && export XDG_DATA_HOME=$HOME/.local/share
[ -f $XDG_DATA_HOME/cargo/env ] && source $XDG_DATA_HOME/cargo/env
[ -f $HOME/.xdg.env ] && . $HOME/.xdg.env

# PATH
export MY_NIX_PROFILE=$HOME/.nix-profile/etc/profile.d/nix.sh
[ -f $MY_NIX_PROFILE ] && . $MY_NIX_PROFILE
[ -f $HOME/.ghcup/env ] && . $HOME/.ghcup/env
[ -d /opt/intel/bin ] && export PATH=/opt/intel/bin:$PATH
export PATH=$HOME/bin:$HOME/.local/bin:$PATH

# string if exists return no stderr if not
function program_exists {
    type $1 > /dev/null 2>&1
}

# Check if shell is in interactive mode
[ $(echo $- | grep i) ] && export IS_INTERACTIVE=yes

# Run tmux
alias tmux="tmux -f $XDG_CONFIG_HOME/tmux/tmux.conf"
if [ "$IS_INTERACTIVE" ]; then
    if program_exists tmux && [ -z "$TMUX" ]; then
        unset SHELL
        export ATTACH_ID="$(tmux ls | grep -vm1 attached | cut -d: -f1 )"
        if [ -z "$ATTACH_ID" ]; then # if not available create a new one
            exec tmux -f $XDG_CONFIG_HOME/tmux/tmux.conf new-session 
        else
            exec tmux -f $XDG_CONFIG_HOME/tmux/tmux.conf attach-session -t "$ATTACH_ID" # if available attach to it
        fi
    fi
fi
[ -n "$TMUX" ] && export SKIM_TMUX=1

#env
export GPG_TTY=$(tty)
export PACMAN=powerpill
export STEAM_RUNTIME=1
export OMP_NUM_THREADS=8
export OPENBLAS_NUM_THREADS=8
export XZ_OPT='-9T0'
export TMPDIR=/tmp
export QT_PLATFORM_PLUGIN=lxqt
export QT_QPA_PLATFORMTHEME=lxqt
export XDG_CURRENT_DESKTOP="LXQt"
export MAKEFLAGS=-j$(($(nproc)-1))
export BAT_THEME="Solarized (dark)"
export LESS="-iFRX"
program_exists vivid && export LS_COLORS="$(vivid generate solarized-dark)"
export SKIM_DEFAULT_COMMAND="fd --type f --color=always || git ls-tree -r --name-only HEAD || rg --files || find ."
export SKIM_DEFAULT_OPTIONS="--ansi"
export FZF_DEFAULT_OPTS="--ansi" 
export FZF_DEFAULT_COMMAND="$SKIM_DEFAULT_COMMAND"
export FZF_CTRL_T_COMMAND="$SKIM_DEFAULT_COMMAND -L \$dir" 
export SKIM_CTRL_T_COMMAND="$FZF_CTRL_T_COMMAND"
program_exists bat && export SKIM_CTRL_T_OPTS="--preview 'bat --style=numbers --color=always --line-range :500 {}'" && export FZF_CTRL_T_OPTS="$SKIM_CTRL_T_OPTS"

# default programs
export VISUAL=vim
export EDITOR=vim
program_exists nvim && export EDITOR=$(which nvim) && export VISUAL=$(which nvim) && alias vim="$(which nvim)" && export MANPAGER="nvim -R +Man!"
program_exists alacritty && export TERMINAL=$(which alacritty)
program_exists fish && export SHELL=$(which fish)
# askpass
program_exists ssh-askpass && export SUDO_ASKPASS=$(which ssh-askpass)
program_exists x11-ssh-askpass && export SUDO_ASKPASS=$(which x11-ssh-askpass)
program_exists openssh-askpass && export SUDO_ASKPASS=$(which openssh-askpass)
program_exists lxqt-openssh-askpass && export SUDO_ASKPASS=$(which lxqt-openssh-askpass)
program_exists lxqt-openssh-askpass && export SUDO_ASKPASS=$(which lxqt-openssh-askpass)

# SSH agent
if ! pgrep -u "$USER" ssh-agent > /dev/null; then
    ssh-agent > "$XDG_RUNTIME_DIR/ssh-agent.env"
fi
if [ -z "$SSH_AUTH_SOCK" ]; then
    source "$XDG_RUNTIME_DIR/ssh-agent.env" >/dev/null
fi

# progs
export GNUPGHOME=$XDG_DATA_HOME/gnupg
export WINEPREFIX=$XDG_CONFIG_HOME/wine
export LESSHISTFILE=$XDG_CACHE_HOME/less/lesshst
export PYLINTHOME=$XDG_DATA_HOME/pylint.d
export CARGO_HOME=$XDG_DATA_HOME/cargo
export JULIA_DEPOT_PATH=$XDG_DATA_HOME/julia
export IPYTHONDIR=$XDG_DATA_HOME/ipython
export BASH_ENV=$XDG_CONFIG_HOME/bash/bash_env
export ENV=$BASH_ENV
export HISTFILE=$XDG_CACHE_HOME/bash/bash_history
export SPACEPY=$XDG_CONFIG_HOME
export SHIV_ROOT=$XDG_DATA_HOME/shiv
export AUDACITY_PATH=$XDG_DATA_HOME/audacity
export IDL_PATH=+$XDG_DATA_HOME/idl:'<IDL_DEFAULT>'
export IDL_DLM_PATH=+$XDG_DATA_HOME/idl:'<IDL_DEFAULT>'
export RIPGREP_CONFIG_PATH=$XDG_CONFIG_HOME/ripgreprc

# welcome message
if program_exists neofetch; then
    if program_exists fortune; then
        welcome="$(fortune)"
        if program_exists cowsay; then
            welcome=$(cowsay -f tux -W30 "$welcome")
        fi
    fi
    neofetch --memory_display infobar --disable term_font de resolution --ascii "$welcome"
fi
[ -f /tmp/weather ] && cat /tmp/weather

# Per computer options
extend=$HOME/.profile.extend.sh
[ -f $extend ] && . $extend

# Run custom shell
if [ "$IS_INTERACTIVE" ] && [ "$SHELL" ]; then
    exec $SHELL -l
fi
