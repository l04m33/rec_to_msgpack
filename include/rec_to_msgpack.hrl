-ifndef(__REC_TO_MSGPACK_HRL__).
-define(__REC_TO_MSGPACK_HRL__, true).

-compile([{parse_transform, rec_to_msgpack}]).

%% Msgpack type rules:
%% erlang               msgpack
%%
%% integer()            int*
%% float()              float/double
%% nil                  nil
%% boolean()            boolean
%% binary()             str
%% list()               array
%%

-type mp_int()              :: integer().
-type mp_float()            :: float().
-type mp_nil()              :: nil.
-type mp_bool()             :: boolean().
-type mp_str()              :: binary().
-type mp_array()            :: list().
-type mp_array(ElemType)    :: list(ElemType).
-type mp_map()              :: {[tuple()]}.

-endif.

