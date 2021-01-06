# Defined in - @ line 1
function pacinstall --wraps='yay --pacman powerpill -Sy' --description 'alias pacinstall=yay --pacman powerpill -Sy'
  yay --pacman powerpill -Sy $argv;
end
