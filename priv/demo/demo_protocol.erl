-module(demo_protocol).

-include("rec_to_msgpack.hrl").

-record(pt_time_flag, {
        type  = 0       :: mp_int(),
        value = 0       :: mp_int()
       }).

-record(pt_time_entry, {
        time_zone = 0   :: mp_int(),
        time = 0        :: mp_int(),
        flags = []      :: mp_array(#pt_time_flag{})
       }).

-record(pt_time_i, {}).
-record(pt_time_o, {
        times = []  :: mp_array(#pt_time_entry{}),
        flag  = 0   :: mp_int()
       }).

