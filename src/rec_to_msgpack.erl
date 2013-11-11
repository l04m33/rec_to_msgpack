-module(rec_to_msgpack).

-export([rec_list_to_bin_list/2]).
-export([bin_list_to_rec_list/3]).
-export([parse_transform/2]).


rec_list_to_bin_list(List, PackFun) ->
    rec_list_to_bin_list(List, PackFun, []).


rec_list_to_bin_list([Rec | Rest], PackFun, AccList) ->
    Res = PackFun(Rec),
    rec_list_to_bin_list(Rest, PackFun, [Res | AccList]);
rec_list_to_bin_list([], _PackFun, AccList) ->
    lists:reverse(AccList).


bin_list_to_rec_list(List, RecName, UnpackFun) ->
    bin_list_to_rec_list(List, RecName, UnpackFun, []).


bin_list_to_rec_list([Bin | Rest], RecName, UnpackFun, AccList) ->
    {Res, <<>>} = UnpackFun(RecName, Bin),
    bin_list_to_rec_list(Rest, RecName, UnpackFun, [Res | AccList]);
bin_list_to_rec_list([], _RecName, _UnpackFun, AccList) ->
    lists:reverse(AccList).


parse_transform(Forms, _Options) ->
    io:format("~p", [Forms]),
    RecMeta = parse_records(Forms),
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
            {MPType, MPRecName};
        mp_array    ->
            [ArrayElemType | _] = MPTypeArgs,
            {MPType, parse_type(ArrayElemType)}
    end.


%% =================== packer stuff ====================

gen_packer_forms(RecMeta) ->
    InitForm = file_attribute(?FILE, ?LINE),
    FunForm = gen_packer_fun_forms(RecMeta, []),
    [InitForm, FunForm].


gen_packer_fun_forms([{RecName, FieldsSpec} | RestRecMeta], Acc) ->
    ClauseBody = gen_packer_clause(FieldsSpec, RecName, []),
    FunClause = clause([var('Rec', ?LINE)],
                       [[call(atom(is_record, ?LINE),
                              [var('Rec', ?LINE), atom(RecName, ?LINE)], ?LINE)]],
                       ClauseBody, ?LINE),
    gen_packer_fun_forms(RestRecMeta, [FunClause | Acc]);
gen_packer_fun_forms([], Acc) ->
    function(pack, 1, Acc, ?LINE).


gen_packer_clause([{FieldName, _Idx, MPType} | RestFields], RecName, Acc) ->
    NewEntry = case MPType of
        mp_int ->
            simple_bin_element(RecName, FieldName, ?LINE);
        mp_float ->
            simple_bin_element(RecName, FieldName, ?LINE);
        mp_nil ->
            simple_bin_element(RecName, FieldName, ?LINE);
        mp_bool ->
            simple_bin_element(RecName, FieldName, ?LINE);
        mp_str ->
            simple_bin_element(RecName, FieldName, ?LINE);
        mp_map ->
            simple_bin_element(RecName, FieldName, ?LINE);
        {record, _SubRecName} ->
            {bin_element, ?LINE,
             call(atom(pack, ?LINE),
                  [var_rec_field('Rec', RecName, FieldName, ?LINE)], ?LINE),
             default, [binary]};
        {mp_array, {record, _ElemRecName}} ->
            {bin_element, ?LINE,
             call(remote(msgpack, pack, ?LINE),
                  [call(remote(rec_to_msgpack, rec_list_to_bin_list, ?LINE),
                        [var_rec_field('Rec', RecName, FieldName, ?LINE),
                         {'fun', ?LINE, {function, pack, 1}}], ?LINE)], ?LINE),
             default, [binary]};
        {mp_array, _} ->
            simple_bin_element(RecName, FieldName, ?LINE)
    end,
    gen_packer_clause(RestFields, RecName, [NewEntry | Acc]);
gen_packer_clause([], _RecName, Acc) ->
    [{bin, ?LINE, lists:reverse(Acc)}].


simple_bin_element(RecName, FieldName, Line) ->
    {bin_element, Line,
        call_msgpack_pack(var_rec_field('Rec', RecName, FieldName, ?LINE), ?LINE),
        default, [binary]}.


call_msgpack_pack(Arg, Line) ->
    call(remote(msgpack, pack, Line), [Arg], Line).


%% =================== unpacker stuff ====================

gen_unpacker_forms(RecMeta) ->
    InitForm = file_attribute(?FILE, ?LINE),
    FunForm = gen_unpacker_fun_forms(RecMeta, []),
    [InitForm, FunForm].


gen_unpacker_fun_forms([{RecName, FieldsSpec} | RestRecMeta], Acc) ->
    ClauseBody = gen_unpacker_clause(FieldsSpec, FieldsSpec, RecName, []),
    FunClause = clause([atom(RecName, ?LINE), var('_Bin1', ?LINE)], [], ClauseBody, ?LINE),
    gen_unpacker_fun_forms(RestRecMeta, [FunClause | Acc]);
gen_unpacker_fun_forms([], Acc) ->
    function(unpack, 2, Acc, ?LINE).


gen_unpacker_clause([{FieldName, Idx, MPType} | RestFields],
                    AllFields, RecName, Acc) ->
    NewEntry = case MPType of
        mp_int ->
            match_simple_unpacked(FieldName, Idx, ?LINE);
        mp_float ->
            match_simple_unpacked(FieldName, Idx, ?LINE);
        mp_nil ->
            match_simple_unpacked(FieldName, Idx, ?LINE);
        mp_bool ->
            match_simple_unpacked(FieldName, Idx, ?LINE);
        mp_str ->
            match_simple_unpacked(FieldName, Idx, ?LINE);
        mp_map ->
            match_simple_unpacked(FieldName, Idx, ?LINE);
        {record, SubRecName} ->
            match(
                tuple([var(FieldName, ?LINE), var(list_to_atom("_Bin" ++ integer_to_list(Idx)), ?LINE)], ?LINE),
                call(atom(unpack, ?LINE), [atom(SubRecName, ?LINE), var(list_to_atom("_Bin" ++ integer_to_list(Idx - 1)), ?LINE)], ?LINE),
                ?LINE);
        {mp_array, {record, ElemRecName}} ->
            ArrVarName = list_to_atom(atom_to_list(FieldName) ++ "_arr"),
            {block, ?LINE, [
                match(
                    tuple([var(ArrVarName, ?LINE), var(list_to_atom("_Bin" ++ integer_to_list(Idx)), ?LINE)], ?LINE),
                    call_msgpack_unpack_stream({var, ?LINE, list_to_atom("_Bin" ++ integer_to_list(Idx - 1))}, ?LINE),
                    ?LINE),
                match(
                    var(FieldName, ?LINE),
                    call(remote(rec_to_msgpack, bin_list_to_rec_list, ?LINE),
                         [var(ArrVarName, ?LINE), atom(ElemRecName, ?LINE),
                          {'fun', ?LINE, {function, unpack, 2}}], ?LINE),
                    ?LINE)]};
        {mp_array, _} ->
            match_simple_unpacked(FieldName, Idx, ?LINE)
    end,
    gen_unpacker_clause(RestFields, AllFields, RecName, [NewEntry | Acc]);
gen_unpacker_clause([], AllFields, RecName, Acc) ->
    VarIdx = length(AllFields) + 1,
    VarName = var(list_to_atom("_Bin" ++ integer_to_list(VarIdx)), ?LINE),
    RecordDef = gen_record_def(RecName, AllFields),
    lists:reverse([tuple([RecordDef, VarName], ?LINE) | Acc]).


gen_record_def(RecName, AllFields) ->
    gen_record_def(RecName, AllFields, []).


gen_record_def(RecName, [{FieldName, _Idx, _MPType} | Rest], Acc) ->
    NewEntry = {record_field, ?LINE,
                atom(FieldName, ?LINE),
                var(FieldName, ?LINE)},
    gen_record_def(RecName, Rest, [NewEntry | Acc]);
gen_record_def(RecName, [], Acc) ->
    {record, ?LINE, RecName, lists:reverse(Acc)}.


match_simple_unpacked(FieldName, VarIdx, Line) ->
    match(
        tuple([var(FieldName, Line), var(list_to_atom("_Bin" ++ integer_to_list(VarIdx)), Line)], ?LINE),
        call_msgpack_unpack_stream({var, Line, list_to_atom("_Bin" ++ integer_to_list(VarIdx - 1))}, Line),
        Line).


call_msgpack_unpack_stream(Arg, Line) ->
    call(remote(msgpack, unpack_stream, Line), [Arg], Line).

call(FunName, Args, Line) ->
    {call, Line, FunName, Args}.

remote(Mod, Fun, Line) ->
    {remote, Line, {atom, Line, Mod}, {atom, Line, Fun}}.

atom(Name, Line) ->
    {atom, Line, Name}.

var(Name, Line) ->
    {var, Line, Name}.

clause(VarList, Guards, Body, Line) ->
    {clause, Line, VarList, Guards, Body}.

function(Name, Arity, Clauses, Line) ->
    {function, Line, Name, Arity, Clauses}.

var_rec_field(VarName, RecName, FieldName, Line) ->
    {record_field, Line, {var, Line, VarName}, RecName, {atom, Line, FieldName}}.

match(Left, Right, Line) ->
    {match, Line, Left, Right}.

file_attribute(FileName, Line) ->
    {attribute, Line, file, {FileName, Line}}.

tuple(Elems, Line) ->
    {tuple, Line, Elems}.

