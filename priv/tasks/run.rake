def root
  File.dirname(__FILE__)  + "/../../"
end
def server_name 
  "stoplight"
end

namespace :server do
  1.upto(4) do |i| 
    desc "start server #{i}"
    task "start#{i}" => [:compile, :boot] do
      sh "erl -pa #{root}/ebin -pa #{root}/deps/*/ebin -name #{server_name}_srv -s reloader -boot #{server_name}", :verbose => true
    end
  end
end

namespace :client do
  task :start => [:compile, :boot] do
    # hostname = %x{basename `hostname` .local}.strip
    # hostname = %x{hostname}.strip
    hostname = "YPCMC05684.yellowpages.local" # bah
    sh %Q{
        erl -pa #{root}/ebin -pa #{root}/deps/*/ebin \
        -name #{server_name}_client \
        -stoplight '#{hostname}' \
        -s reloader \
        -run #{server_name}_client 
    }, :verbose => true
  end
end
