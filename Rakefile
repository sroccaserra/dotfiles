##
# Rake FTW

require 'fileutils'

task :default => [:test]

task :test do
    puts 'Hello Rake!'
end

task :os_independant => [:files_to_source, :git_projects, :useful_commands]

task :linux => [:linux_useful_commands, :linux_files_to_symlink,
                :os_independant] do
    if not File.exists? "~/.bash_profile" and File.exists? "~/.profile"
        sh "cp ~/.profile ~/.bash_profile"
    end

    files_to_source = {
        "~/.bash_profile" => "source ~/dotfiles/bash_profile",
        "~/.bashrc" => "source ~/dotfiles/bashrc",
    }
    files_to_source.each do |key, source_directive|
        filename = File.expand_path key
        FileUtils.touch filename
        put_sroccaserra_section filename, source_directive
    end
end

task :windows => [:os_independant] do
    puts
    sh 'if not defined HOME (setx HOME "%USERPROFILE%")'

    target_dir = File.expand_path '~/Dropbox/.ssh'
    link_dir = File.expand_path '~/.ssh'
    if not File.exists?(link_dir) and File.exists?(target_dir)
        mklink link_dir, target_dir
    end

    if test_command('emacs --version')
        result = `emacs -Q -batch --eval="(message exec-directory)" 2>&1`
        emacs_exec_directory = result.strip
        if not File.exists? emacs_exec_directory + '/emacsw.bat'
            sh "copy emacsw.bat \"#{emacs_exec_directory}\""
        end
    end

    test_command 'es /?', 'You should add Everything (http://www.voidtools.com) to your path.'
end

task :useful_commands do
    test_command 'curl --version'
    test_command 'vim +q'
end

task :linux_useful_commands do
    test_command 'python terminal-colors -xc'
    test_command 'wget --version'
end

task :files_to_source do
    files_to_source = {
        "~/.vimrc" => "source ~/dotfiles/vimrc",
        "~/.emacs"=> '(load-file "~/dotfiles/emacs")'
    }
    files_to_source.each do |key, source_directive|
        file_path = File.expand_path key

        if not File.exists? file_path
            File.open(file_path, "w") do |file|
	        file.write source_directive
	    end
        end
    end
end

directory File.expand_path "~/.vim/bundle"
directory File.expand_path "~/developer"

task :git_projects => [File.expand_path("~/.vim/bundle"),
                       File.expand_path('~/developer')] do
    if not test_command 'git --version'
        puts 'Git is unavailable, git projects skipped.'
        return
    end

    git_projects = {
        'https://github.com/gmarik/vundle.git' => File.expand_path('~/.vim/bundle/vundle'), 
        'git@github.com:sroccaserra/emacs.git' => File.expand_path('~/developer/emacs'),
        'git@github.com:sroccaserra/smart-tab.git' => File.expand_path('~/developer/smart-tab'),
    }
    
    git_projects.each do |url, destination_dir|
        if not test_command("git --git-dir=\"#{destination_dir}/.git\" --work-tree=\"#{destination_dir}\" status")
            sh "git clone #{url} \"#{destination_dir}\""
        end
    end
end

task :linux_files_to_symlink do
    files_to_symlink = {
        "~/.bash_aliases" => "bash_aliases",
        "~/.inputrc" => "inputrc",
        "~/.noserc" => "noserc",
        "~/.tmux.conf" => "tmux.conf"
    }
    files_to_symlink.each do |key, source|
        source_path = File.expand_path source
        link_path = File.expand_path key
        puts link_path
        if not File.exists? link_path
            sh "ln -s #{source_path} #{link_path}"
        end
    end
end

def test_command(command, fail_message="You should add #{command.split(' ')[0]} to your path.")
    puts
    puts "$ #{command}"
    result = system(command)
    if not result
        puts "KO. #{fail_message}"
    end
    result
end

def mklink(link_dir, target_dir)
    puts
    arguments = "\"#{link_dir}\" \"#{target_dir}\""
    case 
    when system('mklink /?')
        sh "mklink /d #{arguments}" 
    when system('junction /?')
        sh "junction #{arguments}"
    end
end

def put_sroccaserra_section(filename, section_contents)
    section_begin = '# begin sroccaserra'
    section_end = '# end sroccaserra'

    sh "sed -i '/#{section_begin}/,/#{section_end}/d' \"#{filename}\""

    File.open(filename, "a") do |file|
        file.puts section_begin
        file.puts section_contents
        file.puts section_end
    end
end
