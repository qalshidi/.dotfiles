# Defined in - @ line 1
function weather --wraps="curl 'https://wttr.in/?m'" --description 'Show local weather'
  curl 'https://wttr.in/?m' $argv;
end
