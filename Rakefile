rule '.beam' => '.erl' do |t|
    puts "Compiling #{t.source} => #{t.name}"
    sh "erlc #{t.source}"
end

rule '.js' => '.coffee' do |t|
    puts "Compiling #{t.source} => #{t.name}"
    sh "coffee --bare --compile #{t.source}"
end

desc 'Compile Erlang source'
FileList['*.erl'].ext.each do |x|
    multitask erl: "#{x}.beam"
end

desc 'Compile Coffee source'
FileList['*.coffee'].ext.each do |x|
    multitask coffee: "#{x}.js"
end

if File.exists? '/usr/lib/yaws/ebin/json.beam'
    rule 'json.beam' do
        sh "cp /usr/lib/yaws/ebin/json.beam ."
    end
else
    rule 'json.beam' do
        puts <<-MSG
        Couldn't find json.beam at
            /usr/lib/yaws/ebin/json.beam
        Try installing erlang-yaws.
        If it's already installed, json.beam
        may be located in a different location
        on your system, and you'll need to copy
        it manually.
        MSG
    end
end

desc 'Get json.beam dependency'
task json: 'json.beam'

desc 'Make escript executable'
task :chmod do
    File.chmod 0744, 'img.escript'
end

desc 'Compile all files'
multitask compile: [:erl, :coffee]

desc 'All the things'
multitask default: [:compile, :json, :chmod]

