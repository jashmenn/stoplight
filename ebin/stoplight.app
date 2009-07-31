{application, stoplight, [
        {description, "Stoplight"},
        {vsn, "0.1"},
        {modules, [stoplight]},
        {env, [
          {port, 8648},
          {servers,[undefined]}
        ]},
       % {registered, [stoplight]},
        {registered, []},
        {applications, [kernel, stdlib]},
        {mod, {stoplight, []}}
]}.
