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

    target_dir = File.expand_path '~/Dropbox/.ssh'
    link_dir = File.expand_path '~/.ssh'
    if not File.exists?(link_dir) and File.exists?(target_dir)
        mklink link_dir, target_dir
    end

    if system('emacs --version')
        result = `emacs -Q -batch --eval="(message exec-directory)" 2>&1`
        emacs_exec_directory = result.strip
        if not File.exists? emacs_exec_directory + '/emacsw.bat'
            sh "copy emacsw.bat \"#{emacs_exec_directory}\""
        end
    else
        suggest 'You should add Emacs to your path.'
    end

    if not system('es /?')
        suggest 'You should add Everything (http://www.voidtools.com) to your path.'
    end
end

task :os_independant => [:files_to_source, :git_projects, :useful_commands]

task :useful_commands do
    commands_and_arguments = {
        'curl' => '--version',
        'vim' => '+q'
    }

    commands_and_arguments.each do |command_name, arguments|
        if not system("#{command_name} #{arguments}")
            suggest "You should add #{command_name} to your path."
        end
    end
end

task :files_to_source do
    files_to_source = {
        "~/.vimrc" => "source ~/dotfiles/vimrc",
        "~/.emacs"=> '(load-file "~/dotfiles/emacs")'
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
        suggest 'You should add Git to your path'
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

def mklink(link_dir, target_dir)
    arguments = "\"#{link_dir}\" \"#{target_dir}\""
    if system('mklink /?')
        sh "mklink /d #{arguments}" 
        return
    end
    if system('junction /?')
        sh "junction #{arguments}"
        return
    end
end

def suggest(message)
    puts "Suggestion: #{message}"
end
