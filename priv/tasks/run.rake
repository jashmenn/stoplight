def root
  File.dirname(__FILE__)  + "/../../"
end
def server_name 
  "stoplight"
end

namespace :server do
  task :start => [:compile, :boot] do
    sh "erl -pa #{root}/ebin -pa #{root}/deps/*/ebin -sname #{server_name}_srv -s reloader -boot #{server_name}", :verbose => true
  end
end
