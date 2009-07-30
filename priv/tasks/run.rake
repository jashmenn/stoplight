namespace :server do
  task :start do
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
  end
end

# -s reloader -boot hermes 
# erl -sname stoplight_srv -pa ebin/ -run stoplight_srv_sup sterl -sname stoplight_srv -pa ebin/ -run stoplight_srv_sup start
