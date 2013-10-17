## What's this? ##

It's a small helper to specify binary network protocols on top of 
[Msgpack][1] declaratively, using native Erlang records.

## How to use it? ##

1. Include **rec_to_msgpack.hrl** in the begining of your declaration
   module.

2. Declare your protocols with typed records, like this:

    -record(pt_some_protocol, {
            some_int_field  = 0  :: mp_int(),
            some_list       = [] :: mp_array(mp_int())
           }).

   For available field types, see **include/rec_to_msgpack.hrl**.

3. Compile your module with **erlc -pa dir/to/rec_to_msgpack/ebin**,
   since we use the **parse_transform** trick to translate the record
   declarations into pack/unpack functions. You don't need the extra
   **-pa** argument if you're using **rebar**, just make sure your code
   depends on **rec_to_msgpack** in your **rebar.config**.

4. You'll get your module with a **pack/1** function and another
   **unpack/2** function. **pack/1** complies with the following spec:

    -spec pack(Rec) -> Packet when
            Rec :: tuple(),         % Records you declared, in fact.
            Packet :: binary().     % The output packet data.

   And **unpack/2** complies with the following:

    -spec unpack(RecName, Packet) -> {Rec, RestPacket} when
            RecName :: atom(),      % Name of your record.
            Packet :: binary(),     % Input packet data.
            Rec :: tuple(),         % Records you declared.
            RestPacket :: binary(). % The remaining packet data.

5. Fit the pack/unpack functions in your networking/serialization code.

6. Date a girl and tell her what magic you've done :)

