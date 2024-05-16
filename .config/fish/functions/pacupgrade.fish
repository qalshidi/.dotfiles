# Defined in - @ line 1
function pacupgrade --wraps='yay -Syu' --description 'alias pacupgrade=yay -Syu'
  nice --adjustment=10 yay -Syu $argv;
end
