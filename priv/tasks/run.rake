def root
  File.dirname(__FILE__)  + "/../../"
end
def base_name 
  "stoplight"
end
def server_name
  "#{base_name}_srv"
end

HOSTNAME = `hostname`.strip 
SERVER   = "#{server_name}1@#{HOSTNAME}"

namespace :server do
  1.upto(4) do |i| 
    desc "start server #{i}"
    task "start#{i}" => [:compile, :boot] do
      existing_server = i > 1 ? " -stoplight servers '#{SERVER}' " : ""
      sh %Q{erl -pa #{root}/ebin -pa #{root}/deps/*/ebin  \
              -name "#{server_name}#{i}@#{HOSTNAME}" \
              -s reloader \
              #{existing_server} \
              -boot #{base_name} 
      }, :verbose => true
    end
  end
end

namespace :client do
  task :start => [:compile, :boot] do
    # hostname = %x{basename `hostname` .local}.strip
    # hostname = %x{hostname}.strip
    sh %Q{
        erl -pa #{root}/ebin -pa #{root}/deps/*/ebin \
        -name "#{base_name}_client@#{HOSTNAME}" \
        -s reloader \
        -boot #{server_name}_client  \
        -stoplight "#{SERVER}"
    }, :verbose => true
        # -stoplight servers '[{#{server_name}_srv1, "#{hostname}"}]'
  end
end
