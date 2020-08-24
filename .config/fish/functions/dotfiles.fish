# Defined in - @ line 1
function dotfiles --wraps='git --git-dir ~/.dotfiles --work-tree=$HOME' --wraps='git --git-dir ~/.dotfiles.git --work-tree=$HOME' --description 'alias dotfiles=git --git-dir ~/.dotfiles.git --work-tree=$HOME'
  git --git-dir ~/.dotfiles.git --work-tree=$HOME $argv;
end
