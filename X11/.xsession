xrdb -load -merge .Xresources
urxvtd -q -o -f
setxkbmap -option caps:none
xmodmap ~/.Xmodmap
xsetroot -cursor_name left_ptr
xset -b
emacs --daemon
compton -b
udiskie --use-udisks2 --smart-tray --no-file-manager &
feh --bg-scale raphael-schaller-249942.jpg
exec i3
