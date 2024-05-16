# Defined in - @ line 1
function pacremove --wraps='sudo pacman -Rns' --description 'alias pacremove=sudo pacman -Rns'
  nice --adjustment=10 sudo pacman -Rns $argv;
end
