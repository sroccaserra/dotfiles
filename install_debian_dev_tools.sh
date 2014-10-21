#!/bin/bash

aptitude update -y

aptitude install -y linux-headers-$(uname -r) build-essential \
        zlib1g-dev libssl-dev libreadline5-dev \
        curl zip unzip vim rake ntp libevent-dev rlwrap rcconf \
        g++ bison flex libncurses-dev libssh-dev cmake \
        git colormake colordiff dos2unix apt-file lintian linux-tools \
        mysql-client libmysqlclient-dev \
        python-dev python-nose python-unittest2 python-paver \
        python-mysqldb python-sqlalchemy virtualenvwrapper

pip install yanc

