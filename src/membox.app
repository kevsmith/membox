% -*- mode: erlang -*-
{application, membox,
 [{description,  "Redis-like key/value store"},
  {vsn,          "0.1"},
  {modules,      [membox, membox_db, membox_lexer, membox_parser, membox_listener,
                  membox_reponse, membox_worker, membox_sup]},
  {registered,   [membox_sup, membox_listener]},
  {applications, [kernel, stdlib, sasl, crypto]},
  {env, "/etc/membox.config"},
  {mod, {membox, []}}]}.
