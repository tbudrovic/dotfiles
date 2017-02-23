xrdb -load -merge .Xresources
urxvtd -q -o -f
xmodmap ~/.Xmodmap &
xsetroot -cursor_name left_ptr &
xset -b &
emacs --daemon &
compton &
exec i3
