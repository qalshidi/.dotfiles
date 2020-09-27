# For fish 3.2.0 update
function fish_user_key_bindings
   bind \cz 'fg 2>/dev/null; commandline -f repaint'
end
# <A-z> brings process back to foreground
bind \ez 'fg 2>/dev/null; commandline -f repaint'
