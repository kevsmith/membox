-record(membox_entry, {type,
                       expiry=-1,
                       keyid,
                       value}).

-define(IDENTITY_FUN, fun(X) -> X end).
