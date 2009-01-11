{application, enge2,
 [{description, "enge2"},
  {vsn, "0.01"},
  {modules, [
    enge2,
    enge2_app,
    enge2_sup,
    enge2_web,
    enge2_deps
  ]},
  {registered, []},
  {mod, {enge2_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
