{application, ggs,
    [{description, "The Generic Game Server"},
    {vsn, "0.1.0"},
    {modules, [
        ggs_app,
        ggs_sup,
        ggs_dispatcher
            ]},
    {registered, [ggs_sup]},
    {applications, [kernel, stdlib]},
    {mod, {ggs_app, []}}
    ]}.

