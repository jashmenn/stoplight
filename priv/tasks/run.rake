def root
  File.dirname(__FILE__)  + "/../../"
end
def server_name 
  "stoplight"
end

namespace :server do
  task :start => [:compile, :boot] do
    # erlang do
    #   testing true
    #   options :path => "./ebin", :cookie => "chordjerl"
    #  
    #   with_node(:node0, :stop => false) do
    #       namespace :erlang_module do
    #         start
    #       end
    #   end
    # end
    sh "erl -pa #{root}/ebin -pa #{root}/deps/*/ebin -sname #{server_name}_srv -s reloader -boot #{server_name}", :verbose => true
  end
end
