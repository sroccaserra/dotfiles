#!/bin/bash

msgReplacement="if \`tty -s\`; then\n    mesg n\nfi"
sed -i "s/^mesg n[\s]*$/${msgReplacement}/" /root/.profile

PS1='\n${debian_chroot:+($debian_chroot)}\[\033[01;31m\]\u@\h\[\033[00m\] $? \[\033[01;34m\]\w\[\033[00m\]\n\[\033[01;31m\]\$\[\033[00m\] '
if [[ -z `grep '^[\s]*PS1=' /root/.bashrc` ]]
then
    echo >> /root/.bashrc
    echo export EDITOR=vim>> /root/.bashrc
    echo >> /root/.bashrc
    echo PS1=\'${PS1}\' >> /root/.bashrc
    echo >> /root/.bashrc
    echo export LS_OPTIONS=\'--color=auto\' >> /root/.bashrc
    echo eval \"\`dircolors\`\" >> /root/.bashrc
    echo alias ls=\'ls \$LS_OPTIONS\' >> /root/.bashrc
    echo alias ll=\'ls \$LS_OPTIONS\ -l\' >> /root/.bashrc
    echo alias l=\'ls \$LS_OPTIONS\ -lA\' >> /root/.bashrc
    echo >> /root/.bashrc
    echo alias rm=\'rm -i\' >> /root/.bashrc
    echo alias cp=\'cp -i\' >> /root/.bashrc
    echo alias mv=\'mv -i\' >> /root/.bashrc
    echo >> /root/.bashrc
    cp vimrc /root/.vimrc
fi

