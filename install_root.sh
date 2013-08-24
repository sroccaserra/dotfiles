#!/bin/bash

function ask_yes_or_no() {
    read -p "$1 ([y]es or [N]o): "

    case $(echo $REPLY | tr '[A-Z]' '[a-z]') in
        y|yes) echo "yes" ;;
        *)   echo "no" ;;
    esac
}

echo
install_root=$(ask_yes_or_no "Customize root?")

echo
if [[ 'no' == "${install_root}" ]]
then
    echo "Root customization skipped."
    exit 0
fi

echo -n "Shielding calls to mesg ..."
msgReplacement="if \`tty -s\`; then\n    mesg n\nfi"
sed -i "s/^mesg n[\s]*$/${msgReplacement}/" /root/.profile
echo done

echo -n "Removing old bashrc customization ..."
sed -i '/# begin sroccaserra/,/# end sroccaserra/d' /root/.bashrc
echo done

echo -n "Writing bashrc customization ..."
PS1='\n${debian_chroot:+($debian_chroot)}\[\033[01;31m\]\u@\h\[\033[00m\] $? \[\033[01;34m\]\w\[\033[00m\]\n\[\033[01;31m\]\$\[\033[00m\] '
cat <<-EOF >> /root/.bashrc
	# begin sroccaserra

	export EDITOR=vim

	PS1='${PS1}'

	export LS_OPTIONS='--color=auto'
	eval "\`dircolors\`"
	alias ls='ls \$LS_OPTIONS'
	alias ll='ls \$LS_OPTIONS -l'
	alias l='ls \$LS_OPTIONS -lA'

	alias rm='rm -i'
	alias cp='cp -i'
	alias mv='mv -i'

	# end sroccaserra
EOF
echo done

echo -n "Writing vimrc customization ..."
sed -n '/^""" Begin shared with root$/,/^""" End shared with root$/ p' vimrc > /root/.vimrc
echo done

