# Defined in - @ line 1
function pacremove --wraps='pacaur -Rns --color=auto' --description 'alias pacremove=pacaur -Rns --color=auto'
  pacaur -Rns --color=auto $argv;
end
