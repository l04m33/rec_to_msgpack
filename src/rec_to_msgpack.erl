-module(rec_to_msgpack).

-export([parse_transform/2]).


parse_transform(Forms, _Options) ->
    io:format("~p~n", [Forms]),
    RecMeta = parse_records(Forms),
    io:format("~p~n", [RecMeta]),
    PackerForms = gen_packer_forms(RecMeta),
    UnpackerForms = gen_unpacker_forms(RecMeta),

    [{eof, _} = EOF,
     {attribute, _, file, _} = FileAttr
     | RestForms] = lists:reverse(Forms),

    lists:reverse(RestForms) ++ PackerForms
        ++ UnpackerForms ++ [FileAttr, EOF].


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


gen_packer_forms(_) ->
    [].     %% TODO


gen_unpacker_forms(_) ->
    [].     %% TODO

