{application, stoplight, [
        {description, "Stoplight"},
        {vsn, "0.1"},
        {modules, [stoplight]},
        {env, [
          {servers,[undefined]}
        ]},
        {registered, [stoplight]},
        {applications, [kernel, stdlib]},
        {mod, {stoplight, []}}
]}.
