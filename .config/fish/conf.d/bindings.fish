# For fish 3.2.0 update
bind \cz 'fg 2>/dev/null; commandline -f repaint'
# <A-z> brings process back to foreground
bind \ez 'fg 2>/dev/null; commandline -f repaint'

# skim
[ -z $SMIK_TMUX ] && type -q sk-tmux && set skimcmd sk-tmux || set skimcmd sk

if type -q sk
    bind \eg "skim_grep"
    bind \ct "commandline -it \'($skimcmd)\'"
end

function skim_grep
    set result ($skimcmd --ansi -i -c 'rg --color=always --vimgrep {}' \
        | sed 's/"/\\\\"/g')
    sleep 0.001
    nvim +cexpr!" \"$result\"" -
end
