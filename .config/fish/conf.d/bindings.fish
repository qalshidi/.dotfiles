# For fish 3.2.0 update
bind \cz 'fg 2>/dev/null; commandline -f repaint'
# <A-z> brings process back to foreground
bind \ez 'fg 2>/dev/null; commandline -f repaint'

# skim
if type -q skim_key_bindings
    skim_key_bindings
    bind \ed skim-cd-widget
end
