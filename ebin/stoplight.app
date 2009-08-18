{application, stoplight, [
        {description, "Stoplight"},
        {id,"Stoplight"},
        {vsn, "0.1"},
        {modules, [stoplight,stoplight_client,
                   stoplight_listener,stoplight_lobbyist,
                   stoplight_misc,stoplight_request,stoplight_srv,
                   stoplight_srv_sup,stoplight_util]}, 
        {env, [
          {port, 8648},
          {servers,[undefined]}
        ]},
        {registered, [stoplight]},
        {applications, [kernel, stdlib, sasl]},
        {mod, {stoplight, []}}
]}.
