# welcome message
function fish_greeting
  if type -q neofetch

      if type -q fortune
          set -x welcome (fortune)
          if type -q cowsay
              set -x welcome (cowsay -f tux -W30 (string join \n $welcome))
          end

      
      end
      neofetch --backend ascii --source (string join \n $welcome | psub) --memory_display infobar --disable term_font de resolution
  end

  if test -f /tmp/weather
    /usr/bin/cat /tmp/weather
  end
end
