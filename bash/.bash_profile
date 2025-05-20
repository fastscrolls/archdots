#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

export PATH="$PATH:$HOME/.local/bin:$HOME/.scripts"
export EDITOR="vis"
export TERMINAL="alacritty"
export BROWSER="firefox"
export FILEMANAGER="lf"
export READER="zathura"
export OPENER='handlr open'

if [ "$(tty)" = "/dev/tty1" ]; then
	pgrep -x i3 || exec startx
fi
