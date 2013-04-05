#!/bin/bash

aptitude update -y

aptitude install -y linux-headers-$(uname -r) build-essential \
        zlib1g-dev libssl-dev libreadline5-dev \
        curl zip unzip vim rake ntp libevent-dev rlwrap \
        g++ bison flex libncurses-dev libssh-dev cmake \
        git colormake colordiff dos2unix lintian \
        mysql-client \
        python-dev python-nose python-unittest2 python-paver \
        python-mysqldb python-sqlalchemy virtualenvwrapper

DEBIAN_FRONTEND=noninteractive aptitude install -q -y \
        mysql-server

pip install yanc

