# Defined in - @ line 1
function pacclean --wraps='pacremove -Rns (pacman -Qtdq)' --wraps='pacremove -Rns (pacman -Qtdq)' --description 'alias pacclean=pacremove -Rns (pacman -Qtdq)'
  nice --adjustment=10 pacremove -Rns (pacman -Qtdq) $argv;
end
