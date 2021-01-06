# Defined in - @ line 1
function pacsearch --wraps='yay -Ss' --description 'alias pacsearch=yay -Ss'
  yay -Ss $argv;
end
