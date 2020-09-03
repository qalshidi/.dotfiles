# Defined in - @ line 1
function tmux --wraps='tmux -f $XDG_CONFIG_HOME/tmux.conf' --description 'alias tmux=tmux -f $XDG_CONFIG_HOME/tmux.conf'
 command tmux -f $XDG_CONFIG_HOME/tmux.conf $argv;
end
