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

# string if exists return no stderr if not
function program_exists {
    which $1 > /dev/null 2>&1
}

# Run tmux
alias tmux='tmux -f $XDG_CONFIG_HOME/tmux.conf'
[ $(echo $- | grep i) ] \
    && program_exists tmux \
    && [ -z "$TMUX" ] \
    && unset SHELL \
    && export ATTACH_ID="$(tmux ls | grep -vm1 attached | cut -d: -f1 )" \
    && if [ -z "$ATTACH_ID" ]; then # if not available create a new one
        exec tmux -f $XDG_CONFIG_HOME/tmux.conf new-session 
    else
        exec tmux -f $XDG_CONFIG_HOME/tmux.conf attach-session -t "$ATTACH_ID" # if available attach to it
    fi

# PATH
export MY_NIX_PROFILE=$HOME/.nix-profile/etc/profile.d/nix.sh
[ -f $MY_NIX_PROFILE ] && . $MY_NIX_PROFILE
[ -f $HOME/.ghcup/env ] && . $HOME/.ghcup/env
[ -d /opt/intel/bin ] && export PATH=/opt/intel/bin:$PATH
export PATH=$HOME/bin:$HOME/.local/bin:$PATH

#env
export GPG_TTY=$(tty)
export PACMAN=powerpill
export STEAM_RUNTIME=1
export OMP_NUM_THREADS=8
export OPENBLAS_NUM_THREADS=8
export TMPDIR=/tmp
export QT_PLATFORM_PLUGIN=lxqt
export QT_QPA_PLATFORMTHEME=lxqt
export XDG_CURRENT_DESKTOP="LXQt"
export MAKEFLAGS=-j$(($(nproc)+1))
export BAT_THEME="Solarized (dark)"
export LESS="-iFRX"

# default programs
export VISUAL=vim
export EDITOR=vim
program_exists nvim && export EDITOR=$(which nvim) && export VISUAL=$(which nvim) && alias vim="$(which nvim)" && export MANPAGER="nvim +Man! +Goyo +'autocmd User GoyoLeave nested q'"
program_exists alacritty && export TERMINAL=$(which alacritty)
program_exists fish && export SHELL=$(which fish)
program_exists lxqt-openssh-askpass && export SUDO_ASKPASS=$(which lxqt-openssh-askpass)

# progs
export TMUX_TMPDIR=$XDG_CACHE_HOME
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

if [ $(uname) = 'Linux' ]; then

    # X11 specific
    if [[ "$DISPLAY" == ":"* ]]; then
        xrdb -I$XDG_CONFIG_HOME/X11 $XDG_CONFIG_HOME/X11/Xresources
        # space as hyper
        if [ -z "$IS_KEYS_MAPPED" ]; then
            spare_modifier_1="Hyper_L"
            spare_modifier_2="Hyper_R"
            setxkbmap -layout 'us'
            setxkbmap -option ctrl:nocaps
            xmodmap -e "keycode 23 = $spare_modifier_1"
            xmodmap -e "remove mod4 = $spare_modifier_1"
            xmodmap -e "keycode any = Tab"
            xmodmap -e "keycode 51 = $spare_modifier_2 $spare_modifier_2 $spare_modifier_2 $spare_modifier_2"
            xmodmap -e "keycode any = backslash bar backslash bar"
            xcape -t 500 -e "Hyper_L=Tab;Hyper_R=backslash"
        fi
        export IS_KEYS_MAPPED="yes"
    fi

fi

# welcome message
if program_exists neofetch; then
    if program_exists fortune; then
        welcome="$(fortune)"
        if program_exists cowsay; then
            welcome=$(cowsay -W30 "$welcome")
        fi
    fi
    neofetch --memory_display infobar --disable term_font de resolution --ascii "$welcome"
fi
[ -f /tmp/weather ] && cat /tmp/weather

# Per computer options
extend=$HOME/.profile.extend.sh
[ -f $extend ] && . $extend

# Run custom shell
[ $(echo $- | grep i) ] && [ $SHELL ] && exec $SHELL -l

# vim:filetype=bash
