require "rubygems"
require "skelerl"

Rake::TaskManager.class_eval do
  def remove_task(task_name)
    @tasks.delete(task_name.to_s)
  end
end
 
def remove_task(task_name)
  Rake.application.remove_task(task_name)
end

# TODOlist for generator
# add utils/make_boot
# generate an application
 
def ebin_dirs
  Dir[File.dirname(__FILE__) + "/ebin"]  +
  Dir[File.dirname(__FILE__) + "/**/deps/**/ebin"] +
  Dir[File.dirname(__FILE__) + "/test/ebin"]
end

def erl
  "erl"
end

task :boot => [:compile] do
  sh "(cd ebin; erl -pa ebin -noshell -run make_boot write_scripts stoplight)"
end

remove_task :compile
task :compile => [:check_submodules] do
  sh "#{erl} -pa #{ebin_dirs} -noinput +B -eval 'case make:all() of up_to_date -> halt(0); error -> halt(1) end.'", :verbose => true
end

remove_task :clean
desc "Clean the beams from the ebin directory"
task :clean do
  cmd = "rm #{::File.dirname(__FILE__)}/ebin/*.{beam,boot}"
  puts cmd
  Kernel.system cmd
end

remove_task :recompile
task :recompile => ["clean", "compile"]

task :default => [:compile, :boot]

task :rstakeout do
    cmd =  %Q{rstakeout -t 1 -v "rake run_tests --trace" '*/**/*.erl'}
    puts cmd
    exec cmd
end

task :check_submodules do
    %w{gen_cluster gen_server_mock}.each do |mod|
      dir = File.dirname(__FILE__) + "/deps/#{mod}"
        unless File.exists?("#{dir}/README.mkd")
            sh "git submodule update --init", :verbose => true
            unless $? == 0
              puts "#{mod} submodule not found. Please `git submodule update --init`"
              exit 1
            end
        end 
    end 
end 

Dir.glob(File.dirname(__FILE__) + "/priv/tasks/*.rake").each {|f| load f}


