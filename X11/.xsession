xrdb -load -merge .Xresources
urxvtd -q -o -f
setxkbmap -option caps:none
xmodmap ~/.Xmodmap
xsetroot -cursor_name left_ptr
xset -b
emacs --daemon
compton -b
feh --bg-scale MVlfHG1.jpg
exec i3
