# Defined in - @ line 1
function pacupgrade --wraps='yay -Syu' --description 'alias pacupgrade=yay -Syu'
  yay --pacman powerpill -Syu $argv;
end
