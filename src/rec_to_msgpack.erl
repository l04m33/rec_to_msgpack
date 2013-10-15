-module(rec_to_msgpack).

-export([parse_transform/2]).


parse_transform(Forms, _Options) ->
    io:format("~p~n", [Forms]),
    RecMeta = parse_records(Forms),
    io:format("~p~n", [RecMeta]),
    PackerForms = gen_packer_forms(RecMeta),
    UnpackerForms = gen_unpacker_forms(RecMeta),

    [{eof, _} = EOF
     | RestForms] = lists:reverse(Forms),

    lists:reverse(RestForms) ++ PackerForms
        ++ UnpackerForms ++ [EOF].


parse_records(Forms) ->
    parse_records(Forms, []).

parse_records([{attribute, _Line0, type,
                {{record, RecName}, RawFieldTypes, _}} | RestForms], Acc) ->
    FieldTypes = parse_record_fields(RawFieldTypes, 2, []),
    parse_records(RestForms, [{RecName, FieldTypes} | Acc]);
parse_records([_ | RestForms], Acc) ->
    parse_records(RestForms, Acc);
parse_records([], Acc) ->
    Acc.

parse_record_fields([{typed_record_field,
                      {record_field, _Line1,
                       {atom, _Line2, FieldName},
                       _FieldVal}, MPType} | RestFields], CurIdx, Acc) ->
    NewEntry = {FieldName, CurIdx, parse_type(MPType)},
    parse_record_fields(RestFields, CurIdx + 1, [NewEntry | Acc]);
parse_record_fields([], _, Acc) ->
    lists:reverse(Acc).


parse_type({type, _Line0, MPType, MPTypeArgs}) ->
    case MPType of
        mp_int      -> MPType;
        mp_float    -> MPType;
        mp_nil      -> MPType;
        mp_bool     -> MPType;
        mp_str      -> MPType;
        mp_map      -> MPType;
        record      ->
            [{atom, _Line1, MPRecName} | _] = MPTypeArgs,
            {record, MPRecName};
        mp_array    ->
            [ArrayElemType | _] = MPTypeArgs,
            {MPType, parse_type(ArrayElemType)}
    end.


gen_packer_forms(RecMeta) ->
    InitForm = {attribute, ?LINE, file, {?FILE, ?LINE}},
    FunForm = gen_packer_fun_forms(RecMeta, []),
    [InitForm, FunForm].

gen_packer_fun_forms([{RecName, _FieldsSpec} | RestRecMeta], Acc) ->
    ClauseBody = [{atom, ?LINE, 'TODO'}],
    FunClause = {clause, ?LINE,
                 [{var, ?LINE, 'Rec'}],
                 [[{call, ?LINE,
                    {atom, ?LINE, is_record},
                    [{var, ?LINE, 'Rec'}, {atom, ?LINE, RecName}]}]],
                 ClauseBody},
    gen_packer_fun_forms(RestRecMeta, [FunClause | Acc]);
gen_packer_fun_forms([], Acc) ->
    {function, ?LINE, pack, 1, Acc}.


gen_unpacker_forms(RecMeta) ->
    InitForm = {attribute, ?LINE, file, {?FILE, ?LINE}},
    FunForm = gen_unpacker_fun_forms(RecMeta, []),
    [InitForm, FunForm].

gen_unpacker_fun_forms([{RecName, _FieldsSpec} | RestRecMeta], Acc) ->
    ClauseBody = [{atom, ?LINE, 'TODO'}],
    FunClause = {clause, ?LINE,
                 [{var, ?LINE, 'Rec'}],
                 [[{call, ?LINE,
                    {atom, ?LINE, is_record},
                    [{var, ?LINE, 'Rec'}, {atom, ?LINE, RecName}]}]],
                 ClauseBody},
    gen_unpacker_fun_forms(RestRecMeta, [FunClause | Acc]);
gen_unpacker_fun_forms([], Acc) ->
    {function, ?LINE, unpack, 1, Acc}.

