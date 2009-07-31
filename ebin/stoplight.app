{application, stoplight, [
        {description, "Stoplight"},
        {vsn, "0.1"},
        {modules, [stoplight]},
        {env, [
          {port, 8648},
          {servers,[{"0.0.0.0",8648}]}
        ]},
        {registered, [stoplight]},
        {applications, [kernel, stdlib]},
        {mod, {stoplight, []}}
]}.
