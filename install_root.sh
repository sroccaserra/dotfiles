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

rake install_root
