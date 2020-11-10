# Defined in - @ line 1
function cp --wraps='rsync -avP' --description 'alias cp=rsync -avP'
  rsync -avP $argv;
end
