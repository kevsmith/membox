% -*- mode: erlang -*-
{application, membox,
 [{description,  "Redis-like key/value store"},
  {vsn,          "0.1"},
  {modules,      [membox, membox_db, membox_lexer, membox_parser, membox_listener,
                  membox_reponse, membox_worker, membox_sup, membox_util]},
  {registered,   [membox_sup, membox_listener]},
  {applications, [kernel, stdlib, sasl, crypto]},
  {mod, {membox, []}}]}.
