# Initial environment
# ~/.profile
#

# PATH
export MY_NIX_PROFILE=$HOME/.nix-profile/etc/profile.d/nix.sh
[[ -f $MY_NIX_PROFILE ]] && . $MY_NIX_PROFILE
export PATH=$HOME/bin:$HOME/.local/bin:$PATH

#env
export STEAM_RUNTIME=1
export OMP_NUM_THREADS=8
export OPENBLAS_NUM_THREADS=8
export VISUAL=nvim
export EDITOR=nvim
export SHELL=$(whereis fish | awk '{ match($0, "\\S+bin\\S+", bin) } END{ print bin[0] }')
export SUDO_ASKPASS=/usr/bin/lxqt-openssh-askpass
export TMPDIR=/tmp
export QT_QPA_PLATFORMTHEME="qt5ct"

# Change where configuration files go
# xdg
export XDG_CONFIG_HOME=$HOME/.config
export XDG_CACHE_HOME=$HOME/.cache
export XDG_DATA_HOME=$HOME/.local/share
# progs
export GNUPGHOME=$XDG_DATA_HOME/gnupg
export WINEPREFIX=$XDG_CONFIG_HOME/wine
export LESSHISTFILE=$XDG_CACHE_HOME/less/lesshst
export PYLINTHOME=$XDG_DATA_HOME/pylint.d
export CARGO_HOME=$XDG_DATA_HOME/cargo
export JULIA_DEPOT_PATH=$XDG_DATA_HOME/julia
export IPYTHONDIR=$XDG_DATA_HOME/ipython
export BASH_ENV=$XDG_CONFIG_HOME/bash/bashrc
export HISTFILE=$XDG_CACHE_HOME/bash/bash_history
export SPACEPY=$XDG_CONFIG_HOME/spacepy
export SHIV_ROOT=$XDG_DATA_HOME/shiv
export AUDACITY_PATH=$XDG_DATA_HOME/audacity
xrdb -I$XDG_CONFIG_HOME/X11 $XDG_CONFIG_HOME/X11/Xresources
if [ -e /home/qusai/.nix-profile/etc/profile.d/nix.sh ]; then . /home/qusai/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
