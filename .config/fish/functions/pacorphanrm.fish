# Defined in - @ line 1
function pacorphanrm --wraps='pacaur -Rns (pacman -Qtdq)' --description 'alias pacorphanrm=pacaur -Rns (pacman -Qtdq)'
  pacaur -Rns (pacman -Qtdq) $argv;
end
