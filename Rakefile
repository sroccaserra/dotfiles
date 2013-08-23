##
# Rake FTW

require 'pathname'

$DOTFILES_DIR = '~/dotfiles'
$BACKUP_DIR = '~/.dotfiles_backup'

$FILES_TO_SYMLINK = FileList['bash_aliases', 'emacs', 'inputrc', 'noserc', 'tmux.conf',
                    'vimrc']
$FILES_TO_SOURCE = FileList['bash_profile', 'bashrc']
$REQUIRED_COMMANDS = ['curl', 'git', 'python', 'ruby', 'wget']

#####

task :default => [:test]

task :test do
    puts $FILES_TO_SYMLINK
end

task :install => [:os_independant] do
end

task :windows => [:os_independant] do
    sh 'if not defined HOME (setx HOME "%USERPROFILE%")'
end

task :os_independant => [:files_to_source, :git_projects, :useful_commands]

task :useful_commands do
    commands_and_arguments = {
        'curl' => '--version',
        'vim' => '+q'
    }

    commands_and_arguments.each do |command_name, arguments|
        if not system("#{command_name} #{arguments}")
            puts "You should add #{command_name} to your path."
        end
    end
end

task :files_to_source do
    files_to_source = {
        "~/.vimrc" => "source ~/dotfiles/vimrc",
        "~/.emacs"=> '(load-file "~/dotfiles/emacs"'
    }
    files_to_source.each do |key, source_directive|
        file_path = File.expand_path key

        if not File.exists? file_path
          File.write file_path, source_directive
        end
    end
end

directory File.expand_path "~/.vim/bundle"
directory File.expand_path "~/developer"

task :git_projects => [File.expand_path("~/.vim/bundle"),
                       File.expand_path('~/developer')] do
    if not system('git --version')
        puts 'Git is not in your path, git projects skipped.'
        return
    end

    git_projects = {
        'https://github.com/gmarik/vundle.git' => File.expand_path('~/.vim/bundle/vundle'), 
        'git@github.com:sroccaserra/emacs.git' => File.expand_path('~/developer/emacs'),
        'git@github.com:sroccaserra/smart-tab.git' => File.expand_path('~/developer/smart-tab'),
    }
    
    git_projects.each do |url, destination_dir|
        puts destination_dir
        if not system("git --git-dir=\"#{destination_dir}/.git\" --work-tree=\"#{destination_dir}\" status")
            sh "git clone #{url} \"#{destination_dir}\""
        end
    end
end
