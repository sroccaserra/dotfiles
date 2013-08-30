##
# Rake FTW

task :default => [:test]

task :test do
    raise unless home() == File.expand_path('~')
    raise unless home('') == File.expand_path('~')
    raise unless home('developer') == File.expand_path('~/developer')

    test = ["1","22","333", "# begin sroccaserra", "44", "555", "66",
            "# end sroccaserra", "77", "8888"]
    cleared = clear_sroccaserra_section(test)
    raise unless cleared == ["1", "22", "333", "77", "8888"]
end

def home(path='')
    File.expand_path File.join('~', path)
end

task :os_independant => [:files_to_source, :git_global_config, :git_projects, :useful_commands]

task :linux => [:linux_useful_commands,
                :linux_files_to_symlink,
                :customize_virtualenv_prompt,
                :linux_developer_tools,
                :os_independant,
                home(".byobu")] do
    bash_profile = home '.bash_profile'
    profile = home '.profile'
    if not File.exists? bash_profile
        if File.exists? profile
            sh "ln -s #{profile} #{bash_profile}"
        else
            File.open bash_profile do |file|
                file.write 'source ~/.bashrc'
            end
        end
    end

    files_to_source = {
        "~/.bash_profile" => "source ~/dotfiles/bash_profile",
        "~/.bashrc" => "source ~/dotfiles/bashrc",
    }
    files_to_source.each do |key, source_directive|
        filename = File.expand_path key
        touch filename
        put_sroccaserra_section filename, source_directive
    end

    # Copy shared tmux conf to byobu dir.
    sh 'sed -n \'/^### Shared with Byobu ###$/,$ p\' tmux.conf > ~/.byobu/.tmux.conf'
end

task :windows => [:os_independant] do
    puts
    sh 'if not defined HOME (setx HOME "%USERPROFILE%")'

    target_dir = home 'Dropbox/.ssh'
    link_dir = home '.ssh'
    if not File.exists?(link_dir) and File.exists?(target_dir)
        mklink link_dir, target_dir
    end

    if test_command('emacs --version')
        result = `emacs -Q -batch --eval="(message exec-directory)" 2>&1`
        emacs_exec_directory = result.strip
        if not File.exists? File.join(emacs_exec_directory, 'emacsw.bat')
            sh "copy emacsw.bat \"#{emacs_exec_directory}\""
        end
    end

    test_command 'es -n 1 dotfiles', 'You should add Everything (http://www.voidtools.com) to your path.'
end

task :useful_commands => [:git_projects] do
    test_command 'curl --version'
    test_command 'vim --noplugin -N "+set hidden" "+syntax on" +BundleInstall +xa'
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

task :git_global_config do
    puts
    sh 'git config --global color.ui auto'
    sh 'git config --global branch.autosetuprebase always'
    sh 'git config --global push.default tracking'
    sh 'git config --global log.date iso'
    sh 'git config --global alias.c commit'
    sh 'git config --global alias.ca "commit -a"'
    sh 'git config --global alias.d diff'
    sh 'git config --global alias.l "log --decorate --graph"'
    sh 'git config --global alias.s "status -sb"'

    if `git config --global user.name`.empty?
        print "Git global user name: "
        name = STDIN.gets().strip
        sh "git config --global user.name #{name}"

        print "Git global user email: "
        email = STDIN.gets().strip
        sh "git config --global user.email #{email}"
    end
end

task :git_projects => [home('.vim/bundle'),
                       home('developer')] do
    if not test_command 'git --version'
        puts 'Git is unavailable, git projects skipped.'
        return
    end

    git_projects = {
        'https://github.com/gmarik/vundle.git' => '~/.vim/bundle/vundle', 
        'git@github.com:sroccaserra/emacs.git' => '~/developer/emacs',
        'git@github.com:sroccaserra/smart-tab.git' => '~/developer/smart-tab',
    }

    git_projects.each do |url, value|
        destination_dir = File.expand_path value
        if not test_command("git --git-dir=\"#{destination_dir}/.git\" --work-tree=\"#{destination_dir}\" status -sb", "")
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
    files_to_symlink.each do |key, value|
        link_path = File.expand_path key
        source_path = File.expand_path value
        if not File.exists? link_path
            sh "ln -s #{source_path} #{link_path}"
        end
    end
end

task :linux_developer_tools => [home("bin")] do
    if test_command 'java -version' and not test_command "lein version"
        sh '\curl https://raw.github.com/technomancy/leiningen/stable/bin/lein > "$HOME/bin/lein"'
        sh 'chmod +x ~/bin/lein'
    end
end

task :customize_virtualenv_prompt do
    virtualenvs_postactivate = home ".virtualenvs/postactivate"
    if File.exists? virtualenvs_postactivate
        put_sroccaserra_section(
            virtualenvs_postactivate,
            'PS1="\n(`basename \"$VIRTUAL_ENV\"`)$_OLD_VIRTUAL_PS1"'
        )
    end
end

directory home ".byobu"
directory home ".vim/bundle"
directory home "bin"
directory home "developer"

def test_command(command, fail_message="You should add #{command.split(' ')[0]} to your path.")
    puts
    puts "$ #{command}"
    result = system(command)
    if not result and not fail_message.empty?
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

def clear_sroccaserra_section(lines)
    ok = true

    lines.reduce [] do |memo, line|
        if line =~ /# begin sroccaserra/
            ok = false
        end
        keep = ok
        if line =~ /# end sroccaserra/
            ok = true
        end
        keep ? memo + [line] : memo
    end
end

def put_sroccaserra_section(filename, section_contents)
    section_begin = '# begin sroccaserra'
    section_end = '# end sroccaserra'

    file_lines = File.open(filename, "r") do |file|
        clear_sroccaserra_section file
    end

    File.open(filename, "w") do |file|
        file.puts file_lines
        file.puts section_begin
        file.puts section_contents
        file.puts section_end
    end
end

