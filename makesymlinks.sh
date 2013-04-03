#!/bin/bash
############################
# .make.sh
# This script creates symlinks from the home directory to any desired dotfiles in ~/dotfiles
############################

########## Variables

dir=~/dotfiles                    # dotfiles directory
olddir=~/dotfiles_old             # old dotfiles backup directory
files="bash_aliases bash_profile inputrc noserc tmux.conf vimrc"    # list of files/folders to symlink in homedir

##########

# create dotfiles_old in homedir
echo -n "Creating $olddir for backup of any existing dotfiles in ~ ..."
mkdir -p $olddir
echo "done"

# change to the dotfiles directory
echo -n "Changing to the $dir directory ..."
cd $dir
echo "done"

# move any existing dotfiles in homedir to dotfiles_old directory, then create symlinks from the homedir to any files in the ~/dotfiles directory specified in $files
for file in $files; do
    echo "Moving any existing dotfiles from ~ to $olddir"
    mv ~/.$file ~/dotfiles_old/
    echo "Creating symlink to $file in home directory."
    ln -s $dir/$file ~/.$file
done

if [[ -z `grep dotfiles ~/.bashrc` ]]
then
    echo -n "Customizing bashrc ..."
    cat <<-EOF >> ~/.bashrc

		# sroccaserra/dotfiles
		
		if [[ -f ~/dotfiles/bashrc ]]
		then
		    source ~/dotfiles/bashrc
		fi
EOF
    echo done
fi


if [[ -z `git config --global user.name` ]]
then
    git config --global color.ui auto
    read -p "Git global user name: " GIT_USER_NAME
    git config --global user.name $GIT_USER_NAME
fi
if [[ -z `git config --global user.email` ]]
then
    read -p "Git global user email: " GIT_USER_EMAIL
    git config --global user.email $GIT_USER_EMAIL
fi

function choice {
    CHOICE=''
    local prompt="$*"
    local answer
    read -p "$prompt" answer
    case "$answer" in
        [yY1] ) CHOICE='y';;
    [nN0] ) CHOICE='n';;
    * ) CHOICE="$answer";;
    esac
    if [ -z "$CHOICE" ]
    then
        CHOICE='y'
    fi
} # end of function choice

choice "Customize root? [Y/n]: "

if [[ 'y' == "$CHOICE" ]]
then
    sudo ./root.sh
else
    echo "Root customization skipped."
fi


