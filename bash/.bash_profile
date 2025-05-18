#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

export PATH="$PATH:$HOME/.local/bin"
export EDITOR="vis"
export TERMINAL="alacritty"
export BROWSER="librewolf"
export READER="zathura"

if [ "$(tty)" = "/dev/tty1" ]; then
	pgrep -x i3 || exec startx
fi
