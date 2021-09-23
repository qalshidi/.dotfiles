# Defined in - @ line 1
function cp --wraps='rsync -atvP' --description 'alias cp=rsync -atvP'
  rsync -atvP $argv;
end
