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
      sh "erl -pa #{root}/ebin -pa #{root}/deps/*/ebin -name #{server_name}_srv#{i} -s reloader -boot #{server_name}", :verbose => true
    end
  end
end

namespace :client do
  task :start => [:compile, :boot] do
    # hostname = %x{basename `hostname` .local}.strip
    # hostname = %x{hostname}.strip
    hostname = "YPCMC05684.yellowpages.local" # bah, ew
    sh %Q{
        erl -pa #{root}/ebin -pa #{root}/deps/*/ebin \
        -name #{server_name}_client \
        -s reloader \
        -boot #{server_name}_client  \
        -stoplight "#{hostname}"
    }, :verbose => true
        # -stoplight servers '[{#{server_name}_srv1, "#{hostname}"}]'
  end
end
