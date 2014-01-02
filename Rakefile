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
    task erl: "#{x}.beam"
end

desc 'Compile Coffee source'
FileList['*.coffee'].ext.each do |x|
    task coffee: "#{x}.js"
end

task json: 'json.beam'

desc 'Get json.beam dependency'
rule 'json.beam' do
    file = FileList['/usr/lib/yaws/ebin/json.beam']
    raise "You'll need to add 'json.beam' manually...\nNot found.\n" if file.empty?
    sh "cp #{file.first} ."
end

desc 'Make escript executable'
task :chmod do
    File.chmod 0744, 'img.escript'
end

desc 'Compile all files'
multitask compile: [:erl, :coffee]

desc 'All the things'
multitask default: [:compile, :json, :chmod]
