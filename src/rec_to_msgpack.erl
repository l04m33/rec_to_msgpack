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
    InitForm = {attribute, ?LINE, file, {?FILE, ?LINE}},
    FunForm = gen_packer_fun_forms(RecMeta, []),
    [InitForm, FunForm].


gen_packer_fun_forms([{RecName, FieldsSpec} | RestRecMeta], Acc) ->
    ClauseBody = gen_packer_clause(FieldsSpec, RecName, []),
    FunClause = {clause, ?LINE,
                 [{var, ?LINE, 'Rec'}],
                 [[{call, ?LINE,
                    {atom, ?LINE, is_record},
                    [{var, ?LINE, 'Rec'}, {atom, ?LINE, RecName}]}]],
                 ClauseBody},
    gen_packer_fun_forms(RestRecMeta, [FunClause | Acc]);
gen_packer_fun_forms([], Acc) ->
    {function, ?LINE, pack, 1, Acc}.


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
             {call, ?LINE, 
              {atom, ?LINE, pack},
              [{record_field, ?LINE,
                {var, ?LINE, 'Rec'},
                RecName,
                {atom, ?LINE, FieldName}}]},
             default, [binary]};
        {mp_array, {record, _ElemRecName}} ->
            {bin_element, ?LINE,
             {call, ?LINE,
              {remote, ?LINE, {atom, ?LINE, msgpack}, {atom, ?LINE, pack}},
              [{call, ?LINE,
                {remote, ?LINE,
                 {atom, ?LINE, rec_to_msgpack},
                 {atom, ?LINE, rec_list_to_bin_list}},
                [{record_field, ?LINE,
                  {var,?LINE,'Rec'},
                  RecName,
                  {atom, ?LINE, FieldName}},
                 {'fun', ?LINE, {function, pack, 1}}]}]},
             default, [binary]};
        {mp_array, _} ->
            simple_bin_element(RecName, FieldName, ?LINE)
    end,
    gen_packer_clause(RestFields, RecName, [NewEntry | Acc]);
gen_packer_clause([], _RecName, Acc) ->
    [{bin, ?LINE, lists:reverse(Acc)}].





simple_bin_element(RecName, FieldName, Line) ->
    {bin_element, Line,
        call_msgpack_pack({record_field, ?LINE,
                           {var, ?LINE, 'Rec'},
                           RecName,
                           {atom, ?LINE, FieldName}}, ?LINE),
        default, [binary]}.

call_msgpack_pack(Arg, Line) ->
    {call, Line,
     {remote, Line, {atom, Line, msgpack}, {atom, Line, pack}}, [Arg]}.


%% =================== unpacker stuff ====================

gen_unpacker_forms(RecMeta) ->
    InitForm = {attribute, ?LINE, file, {?FILE, ?LINE}},
    FunForm = gen_unpacker_fun_forms(RecMeta, []),
    [InitForm, FunForm].

gen_unpacker_fun_forms([{RecName, FieldsSpec} | RestRecMeta], Acc) ->
    ClauseBody = gen_unpacker_clause(FieldsSpec, FieldsSpec, RecName, []),
    FunClause = {clause, ?LINE,
                 [{atom, ?LINE, RecName}, {var, ?LINE, '_Bin1'}],
                 [],
                 ClauseBody},
    gen_unpacker_fun_forms(RestRecMeta, [FunClause | Acc]);
gen_unpacker_fun_forms([], Acc) ->
    {function, ?LINE, unpack, 2, Acc}.

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
            {match, ?LINE,
             {tuple, ?LINE,
              [{var, ?LINE, FieldName},
               {var, ?LINE, list_to_atom("_Bin" ++ integer_to_list(Idx))}]},
             {call, ?LINE,
              {atom, ?LINE, unpack},
              [{atom, ?LINE, SubRecName},
               {var, ?LINE, list_to_atom("_Bin" ++ integer_to_list(Idx - 1))}]}};
        {mp_array, {record, ElemRecName}} ->
            ArrVarName = list_to_atom(atom_to_list(FieldName) ++ "_arr"),
            {block, ?LINE, [
                {match, ?LINE,
                 {tuple, ?LINE,
                  [{var, ?LINE, ArrVarName},
                   {var, ?LINE, list_to_atom("_Bin" ++ integer_to_list(Idx))}]},
                 call_msgpack_unpack_stream({var, ?LINE, list_to_atom("_Bin" ++ integer_to_list(Idx - 1))}, ?LINE)},

                {match, ?LINE,
                 {var, ?LINE, FieldName},
                 {call, ?LINE,
                  {remote, ?LINE,
                   {atom, ?LINE, rec_to_msgpack},
                   {atom, ?LINE, bin_list_to_rec_list}},
                  [{var, ?LINE, ArrVarName}, {atom, ?LINE, ElemRecName},
                   {'fun', ?LINE, {function, unpack, 2}}]}}]};
        {mp_array, _} ->
            match_simple_unpacked(FieldName, Idx, ?LINE)
    end,
    gen_unpacker_clause(RestFields, AllFields, RecName, [NewEntry | Acc]);
gen_unpacker_clause([], AllFields, RecName, Acc) ->
    VarIdx = length(AllFields) + 1,
    VarName = {var, ?LINE, list_to_atom("_Bin" ++ integer_to_list(VarIdx))},
    RecordDef = gen_record_def(RecName, AllFields),
    lists:reverse([{tuple, ?LINE, [RecordDef, VarName]} | Acc]).

gen_record_def(RecName, AllFields) ->
    gen_record_def(RecName, AllFields, []).

gen_record_def(RecName, [{FieldName, _Idx, _MPType} | Rest], Acc) ->
    NewEntry = {record_field, ?LINE,
                {atom, ?LINE, FieldName},
                {var, ?LINE, FieldName}},
    gen_record_def(RecName, Rest, [NewEntry | Acc]);
gen_record_def(RecName, [], Acc) ->
    {record, ?LINE, RecName, lists:reverse(Acc)}.

match_simple_unpacked(FieldName, VarIdx, Line) ->
    {match, Line,
     {tuple, ?LINE,
      [{var, ?LINE, FieldName},
       {var, ?LINE, list_to_atom("_Bin" ++ integer_to_list(VarIdx))}]},
     call_msgpack_unpack_stream(
            {var, ?LINE, list_to_atom("_Bin" ++ integer_to_list(VarIdx - 1))}, ?LINE)}.

call_msgpack_unpack_stream(Arg, Line) ->
    {call, Line,
     {remote, Line, {atom, Line, msgpack}, {atom, Line, unpack_stream}}, [Arg]}.

