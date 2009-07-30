def root
  File.dirname(__FILE__)  + "/../../"
end
def server_name 
  "stoplight"
end

namespace :server do
  task :start => [:compile, :boot] do
    # sh "erl -pa #{root}/ebin -pa #{root}/deps/*/ebin -sname #{server_name}_srv -s reloader -boot #{server_name}", :verbose => true
    sh "erl -pa #{root}/ebin -pa #{root}/deps/*/ebin -sname #{server_name}_srv -boot #{server_name}", :verbose => true
  end
end

namespace :client do
  task :start => [:compile, :boot] do
    hostname = %x{basename `hostname` .local}.strip
    sh %Q{
        erl -pa #{root}/ebin -pa #{root}/deps/*/ebin \
        -sname #{server_name}_client \
        -stoplight '#{hostname}' \
        -run #{server_name}_client 
    }, :verbose => true
  end
end
