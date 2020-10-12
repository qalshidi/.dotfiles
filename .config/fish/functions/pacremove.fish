# Defined in - @ line 1
function pacremove --wraps='pacaur -Rns --color=auto' --wraps='sudo powerpill -Rns' --description 'alias pacremove=sudo powerpill -Rns'
  sudo powerpill -Rns $argv;
end
