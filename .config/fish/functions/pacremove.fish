# Defined in - @ line 1
function pacremove --wraps='sudo powerpill -Rns' --description 'alias pacremove=sudo powerpill -Rns'
  nice --adjustment=10 sudo powerpill -Rns $argv;
end
