# Defined in - @ line 1
function pacinstall --wraps='yay -Sy' --description 'alias pacinstall=yay -Sy'
  nice --adjustment=10 yay -Sy $argv;
end
