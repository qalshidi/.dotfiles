# Initial environment
# ~/.profile
#

# PATH
export MY_NIX_PROFILE=$HOME/.nix-profile/etc/profile.d/nix.sh
[ -f $MY_NIX_PROFILE ] && . $MY_NIX_PROFILE
[ -f $HOME/.ghcup/env ] && . $HOME/.ghcup/env
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

function program_exists {
    whereis $1 | grep bin
}
# default programs
export VISUAL=vim
export EDITOR=vim
[ -n "$(program_exists nvim)" ] && export EDITOR=$(which nvim) && export VISUAL=$(which nvim)
[ -n "$(program_exists alacritty)" ] && export TERMINAL=$(which alacritty)
[ -n "$(program_exists fish)" ] && export SHELL=$(which fish)
[ -n "$(program_exists lxqt-openssh-askpass)" ] && export SUDO_ASKPASS=$(which lxqt-openssh-askpass)

# Change where configuration files go
# xdg
[ -z $XDG_CONFIG_HOME ] && export XDG_CONFIG_HOME=$HOME/.config
[ -z $XDG_CACHE_HOME ] && export XDG_CACHE_HOME=$HOME/.cache
[ -z $XDG_DATA_HOME ] && export XDG_DATA_HOME=$HOME/.local/share
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
export SPACEPY=$XDG_CONFIG_HOME/spacepy
export SHIV_ROOT=$XDG_DATA_HOME/shiv
export AUDACITY_PATH=$XDG_DATA_HOME/audacity
export IDL_PATH=+$XDG_DATA_HOME/idl:'<IDL_DEFAULT>'
export IDL_DLM_PATH=+$XDG_DATA_HOME/idl:'<IDL_DEFAULT>'

if [ $(uname) = 'Linux' ]; then

    # X11 specific
    if [ "$DISPLAY" ]; then
        xrdb -I$XDG_CONFIG_HOME/X11 $XDG_CONFIG_HOME/X11/Xresources
        # space as hyper
        if [ -z "$IS_KEYS_MAPPED" ]; then
            setxkbmap -option ctrl:nocaps
            xmodmap -e "keycode 23 = Hyper_L"
            xmodmap -e "keycode any = Tab"  
            xmodmap -e "keycode 51 = Hyper_R NoSymbol Hyper_R NoSymbol"
            xmodmap -e "keycode any = backslash bar backslash bar"  
            xcape -e "Hyper_L=Tab;Hyper_R=backslash"
        fi
        export IS_KEYS_MAPPED="yes"
    fi

fi

# welcome message
if [ -n "$(program_exists neofetch)" ]; then
    if [ -n "$(program_exists fortune)" ]; then
        welcome="$(fortune)"
        if [ -n "$(program_exists cowsay)" ]; then
            welcome="$(echo $welcome | cowsay -W30)"
        fi
    fi
    neofetch --memory_display infobar --disable term_font de resolution --ascii "$welcome"
fi

# Per computer options
extend=$HOME/.profile.extend.sh
[ -f $extend ] && . $extend

# Run custom shell
[ $(echo $- | grep i) ] && [ -n "$SHELL" ] && exec $SHELL -l

# vim:filetype=bash
