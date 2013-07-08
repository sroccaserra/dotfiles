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

echo "Copying shared tmux conf to byobu dir."
mkdir -p ~/.byobu
sed -n '/^### Shared with Byobu ###$/,$ p' tmux.conf > ~/.byobu/.tmux.conf

######
## Vim
if [[ ! -f /usr/local/bin/vim ]]
then
    mkdir -p ~/developer
    cd ~/developer; \
        hg clone https://code.google.com/p/vim/; \
        cd vim; \
        ./configure --enable-rubyinterp --enable-pythoninterp --enable-luainterp --with-features=HUGE; \
        colormake; \
        sudo colormake install

    sudo update-alternatives --install /usr/bin/editor editor /usr/local/bin/vim 200 --slave /usr/share/man/man1/editor.1 editor.1 /usr/local/share/man/man1/vim.1
    sudo update-alternatives --install /usr/bin/vi vi /usr/local/bin/vim 200 --slave /usr/share/man/man1/vi.1 vi.1 /usr/local/share/man/man1/vim.1
    sudo update-alternatives --set editor /usr/local/bin/vim
    sudo update-alternatives --set vi /usr/local/bin/vim
fi

mkdir -p ~/.vim/colors
if [[ ! -f ~/.vim/colors/zenburn.vim ]]
then
    echo "Zenburning Vim."
    wget http://www.vim.org/scripts/download_script.php?src_id=15530 -O ~/.vim/colors/zenburn.vim
fi
if [[ ! -d ~/.vim/bundle ]]
then
    echo "Infecting Vim."
    mkdir -p ~/.vim/autoload ~/.vim/bundle; \
        curl -Sso ~/.vim/autoload/pathogen.vim \
            https://raw.github.com/tpope/vim-pathogen/master/autoload/pathogen.vim
fi
if [[ ! -d ~/.vim/bundle/nerdtree ]]
then
    echo "NERDTree-ing Vim."
    cd ~/.vim/bundle; \
        git clone https://github.com/scrooloose/nerdtree.git
fi
if [[ ! -d ~/.vim/ruby/command-t ]]
then
    echo "CommandT-ing Vim."
    wget http://www.vim.org/scripts/download_script.php?src_id=18167 -O command-t-1.4.vba
    vim -S "command-t-1.4.vba" -c ":q"
    cd ~/.vim/ruby/command-t; \
        ruby extconf.rb; \
        make
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


