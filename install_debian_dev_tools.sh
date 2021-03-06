#!/bin/bash

aptitude update -y

aptitude install -y linux-headers-$(uname -r) build-essential \
        zlib1g-dev libssl-dev libreadline-dev \
        curl zip unzip vim rake ntp libevent-dev rlwrap rcconf \
        g++ bison flex libncurses-dev libssh-dev cmake \
        git colormake colordiff dos2unix apt-file lintian linux-tools \
        python-dev python-nose python-unittest2 python-paver \
        python-sqlalchemy virtualenvwrapper

pip install yanc

