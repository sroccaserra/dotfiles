##
# Rake FTW

task :default => [:test]

task :test do
    raise unless home() == File.expand_path('~')
    raise unless home('') == File.expand_path('~')
    raise unless home('developer') == File.expand_path('~/developer')

    test_lines = ["1","22","333", "# begin sroccaserra", "44", "555", "66",
                  "# end sroccaserra", "77", "8888"]

    sroccaserra_section = read_sroccaserra_section test_lines
    raise unless ['# begin sroccaserra', "44", "555", "66", '# end sroccaserra'] == sroccaserra_section

    outside_sroccaserra = read_outside_sroccaserra_section test_lines
    raise unless outside_sroccaserra == ["1", "22", "333", "77", "8888"]

    section = read_section test_lines, /# begin sroccaserra/, /(?!)/
    raise unless ['# begin sroccaserra', "44", "555", "66", '# end sroccaserra', "77", "8888"] == section
end

def home(path='')
    File.expand_path File.join('~', path)
end

task :os_independant => [:files_to_source,
                         :git_global_config,
                         :git_projects,
                         :useful_commands]

task :linux => [:linux_useful_commands,
                :linux_files_to_symlink,
                :customize_virtualenv_prompt,
                :linux_developer_tools,
                :os_independant,
                home(".byobu"),
                home(".config/fish"),
                home(".ssh")] do
    bash_profile = home '.bash_profile'
    profile = home '.profile'
    if not File.exists? bash_profile
        if File.exists? profile
            sh "ln -s #{profile} #{bash_profile}"
        else
            File.open bash_profile, 'w' do |file|
                file.write 'source ~/.bashrc'
            end
        end
    end

    puts
    files_to_source = {
        home(".bash_profile") => "source #{pwd}/bash_profile",
        home(".bashrc") => "source #{pwd}/bashrc",
        home(".config/fish/config.fish") => [
            "set -x sroccaserra_dotfiles #{pwd}",
            ". #{pwd}/fish/config.fish"
        ]
    }
    files_to_source.each do |file_name, source_directive|
        touch file_name
        put_sroccaserra_section file_name, source_directive
    end
    
    puts
    puts '# Copy shared tmux conf to byobu dir.'
    tmux_shared_conf = File.open 'tmux.conf', 'r'  do |file|
        read_section file, /^### Shared with Byobu ##\#$/, /(?!)/
    end
    File.open home('.byobu/.tmux.conf'), 'w' do |file|
        file.puts tmux_shared_conf
    end
end

task :windows => [:os_independant] do
    puts
    sh 'if not defined HOME (setx HOME "%USERPROFILE%")'

    target_dir = home 'Dropbox/.ssh'
    link_dir = home '.ssh'
    if not File.exists?(link_dir) and File.exists?(target_dir)
        mklink_dir link_dir, target_dir
    end

    if test_command('emacs --version')
        result = `emacs -Q -batch --eval="(message exec-directory)" 2>&1`
        emacs_exec_directory = result.strip
        if not File.exists? File.join(emacs_exec_directory, 'emacsw.bat')
            sh "copy emacsw.bat \"#{emacs_exec_directory}\""
        end
    end

    test_command 'es -n 1 dotfiles',
                 'You should add Everything (http://www.voidtools.com) to your path.'
end

task :useful_commands => [:git_projects] do
    test_command 'curl --version | head -n1'
    test_command 'vim --noplugin -N "+set hidden" "+syntax on" +BundleInstall +xa'
end

task :linux_useful_commands do
    test_command 'python terminal-colors -xc'
    test_command 'wget --version | head -n1'
end

task :files_to_source do
    files_to_source = {
        home(".vimrc") => %Q{exec 'source ' . fnameescape('#{pwd}/vimrc')},
        home(".emacs") => %Q{(load-file "#{pwd}/emacs")}
    }
    files_to_source.each do |file_path, source_directive|
        if not File.exists? file_path
            File.open file_path, 'w' do |file|
                file.write source_directive
            end
        end
    end
end

task :git_global_config do
    puts
    puts "# Git config."

    git_settings = {
       'color.ui' => 'auto',
       'branch.autosetuprebase' => 'always',
       'push.default' => 'tracking',
       'log.date' => 'iso',
       'alias.c' => 'commit',
       'alias.ca' => '"commit -a"',
       'alias.d' => '"diff --word-diff"',
       'alias.l' => '"log --decorate --graph"',
       'alias.s' => '"status -sb"',
       'alias.poule' => 'pull',
       'alias.mouche' => 'push',
       'alias.lapin' => '"log --decorate --graph"'
    }
    git_settings.each do |key, value|
        if not value.gsub(/^"|"$/, '').eql?(`git config --global #{key}`.strip)
            sh "git config --global #{key} #{value}"
        end
    end

    if `git config --global user.name`.empty?
        print "Git global user name: "
        name = STDIN.gets().strip
        sh "git config --global user.name \"#{name}\""
    end

    if `git config --global user.email`.empty?
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
        'https://github.com/gmarik/vundle.git' => home('.vim/bundle/vundle'),
        'git@github.com:sroccaserra/emacs.git' => home('developer/emacs'),
        'git@github.com:sroccaserra/smart-tab.git' => home('developer/smart-tab'),
    }

    git_projects.each do |url, destination_dir|
        if not test_command("git --git-dir=\"#{destination_dir}/.git\" --work-tree=\"#{destination_dir}\" status -sb", "")
            sh "git clone #{url} \"#{destination_dir}\""
        end
    end
end

task :linux_files_to_symlink do
    files_to_symlink = {
        home(".bash_aliases") => "bash_aliases",
        home(".inputrc") => "inputrc",
        home(".noserc") => "noserc",
        home(".ssh/rc") => "ssh_rc",
        home(".tmux.conf") => "tmux.conf",
    }
    files_to_symlink.each do |link_path, value|
        source_path = File.expand_path value
        if not File.exists? link_path
            sh "ln -s #{source_path} #{link_path}"
        end
    end
end

task :linux_developer_tools => [home("bin")] do
    if test_command('java -version 2>&1 | head -n1') && (not test_command("lein version"))
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

task :linux_root => ['/root/.profile', '/root/.bashrc', '/root/.vimrc'] do
    puts "# Shielding calls to mesg."
    ruby %q{-i -pe '$_.gsub!(/^mesg n[\s]*$/, "if `tty -s`; then\n    mesg n\nfi\n")' /root/.profile}

    puts "# Updating bash customization."
    ps1 = '\n${debian_chroot:+($debian_chroot)}\[\e[1;31m\]\u@\h\[\e[m\] $? \[\e[1;33m\]\w\[\e[m\]\n' +
          '\[\e[1;31m\]\$\[\e[m\] '
    put_sroccaserra_section '/root/.bashrc', <<-EOS
        export EDITOR=vim

        PS1='#{ps1}'

        export LS_OPTIONS='--color=auto'
        eval "\`dircolors\`"
        alias ls='ls \$LS_OPTIONS'
        alias ll='ls \$LS_OPTIONS -l'
        alias l='ls \$LS_OPTIONS -lA'

        alias rm='rm -i'
        alias cp='cp -i'
        alias mv='mv -i'
    EOS

    puts "# Writing Vim customization."
    vim_lines = File.open 'vimrc', 'r' do |file|
        read_section file, /^""" Begin shared with root$/, /^""" End shared with root$/
    end
    File.open '/root/.vimrc', 'w' do |file|
        file.puts vim_lines
    end
end

directory home ".byobu"
directory home ".config/fish"
directory home ".ssh"
directory home ".vim/bundle"
directory home "bin"
directory home "developer"

file '/root/.profile'
file '/root/.bashrc'
file '/root/.vimrc'

def test_command(command, fail_message="You should add #{command.split(' ')[0]} to your path.")
    puts
    puts "$ #{command}"
    result = system(command)
    if not result and not fail_message.empty?
        puts "KO. #{fail_message}"
    end
    result
end

def mklink_dir(link_dir, target_dir)
    puts
    arguments = "\"#{link_dir}\" \"#{target_dir}\""
    case
    when system('mklink /?')
        sh "mklink /d #{arguments}"
    when system('junction /?')
        sh "junction #{arguments}"
    end
end

def read_section(lines, begin_expression, end_expression, inside=true)
    status = !inside

    lines.reduce [] do |memo, line|
        if line =~ begin_expression
            status = !status
        end
        keep = status
        if line =~ end_expression
            status = !status
        end
        keep ? memo + [line] : memo
    end
end

def read_sroccaserra_section(lines)
    read_section lines, /# begin sroccaserra/, /# end sroccaserra/
end

def read_outside_sroccaserra_section(lines)
    read_section lines, /# begin sroccaserra/, /# end sroccaserra/, false
end

def put_sroccaserra_section(filename, section_contents)
    file_lines = File.open filename, 'r' do |file|
        read_outside_sroccaserra_section file
    end

    File.open filename, 'w' do |file|
        file.puts file_lines
        file.puts '# begin sroccaserra'
        file.puts section_contents
        file.puts '# end sroccaserra'
    end
end
