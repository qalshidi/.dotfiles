# Defined in - @ line 1
function pacinstall --wraps='pacaur -Sy --noedit --color=auto' --wraps='yay --pacman powerpill -Sy' --description 'alias pacinstall=yay --pacman powerpill -Sy'
  yay --pacman powerpill -Sy $argv;
end
