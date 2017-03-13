xrdb -load -merge .Xresources
urxvtd -q -o -f
xmodmap ~/.Xmodmap
xsetroot -cursor_name left_ptr
xset -b
emacs --daemon
compton -b
feh --bg-scale 1964-11.jpg
exec i3
