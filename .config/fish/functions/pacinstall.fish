# Defined in - @ line 1
function pacinstall --wraps='pacaur -Sy --noedit --color=auto' --wraps='pacaur -Sy --noedit --color=auto' --description 'alias pacinstall=pacaur -Sy --noedit --color=auto'
  pacaur -Sy --noedit --color=auto $argv;
end
