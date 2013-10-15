-ifndef(__DEMO_PROTOCOL_HRL__).
-define(__DEMO_PROTOCOL_HRL__, true).

-record(pt_time_entry, {
        time_zone = 0   :: mp_int(),
        time = 0        :: mp_int()
       }).

-record(pt_time_i, {}).
-record(pt_time_o, {
        times = [] :: mp_array(record(pt_time_entry))
       }).

-endif.
