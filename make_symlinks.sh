#!/bin/bash
############################
# .make.sh
# This script creates symlinks from the home directory to any desired dotfiles in ~/dotfiles
############################

########## Variables

dir=~/dotfiles                    # dotfiles directory
olddir=~/dotfiles_old             # old dotfiles backup directory
files_to_symlink="bash_aliases inputrc noserc tmux.conf vimrc"    # list of files/folders to symlink in homedir
files_to_source="bash_profile bashrc"    # list of files/folders to source

##########

# create dotfiles_old in homedir
echo -n "Creating $olddir for backup of any existing dotfiles in ~ ..."
mkdir -p $olddir
echo "done"

# change to the dotfiles directory
echo -n "Changing to the $dir directory ..."
cd $dir
echo "done"

# Make sure .bash_profile is used
if [[ ! -f ~/.bash_profile && -f ~/.profile ]]
then
    echo -n "Copying ~/.profile to ~/.bash_profile ..."
    cp ~/.profile ~/.bash_profile
    echo done
fi

# move any existing dotfiles in homedir to dotfiles_old directory, then create symlinks from the homedir to any files in the ~/dotfiles directory specified in $files
for file in $files_to_symlink; do
    echo "Moving any existing dotfiles from ~ to $olddir"
    mv ~/.$file ~/dotfiles_old/
    echo "Creating symlink to $file in home directory."
    ln -s $dir/$file ~/.$file
done

function remove_sroccaserra_section {
    local file_path="$1"
    sed -i '/# begin sroccaserra/,/# end sroccaserra/d' ${file_path}
}

function insert_source_directive {
    local file_to_source="$1"
    cat <<-EOF >> ~/.${file_to_source}
		# begin sroccaserra
		if [[ -f ~/dotfiles/${file_to_source} ]]
		then
		    source ~/dotfiles/${file_to_source}
		fi
		# end sroccaserra
EOF
}

for file in ${files_to_source}
do
    echo -n "Removing old ~/.${file} source directive ..."
    remove_sroccaserra_section ~/.${file}
    echo done
    echo -n "Inserting new source directive in ~/.${file} ..."
    insert_source_directive "${file}"
    echo done
done

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

mkdir -p ~/.virtualenvs
if [[ -f ~/.virtualenvs/postactivate ]]
then
    remove_sroccaserra_section ~/.virtualenvs/postactivate
    cat <<-'EOF' >> ~/.virtualenvs/postactivate
		# begin sroccaserra
		PS1="\n(`basename \"$VIRTUAL_ENV\"`)$_OLD_VIRTUAL_PS1"
		# end sroccaserra
EOF
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
    sudo ./customize_root.sh
else
    echo "Root customization skipped."
fi


