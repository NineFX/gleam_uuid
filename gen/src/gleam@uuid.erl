-module(gleam@uuid).
-compile(no_auto_import).

-export([v1/0, v1_string/0, v1_custom/2, v4/0, v4_string/0, v5/2, version/1, variant/1, time/1, time_posix_microsec/1, clock_sequence/1, node/1, to_string/1, format/2, from_string/1, dns_uuid/0, url_uuid/0, oid_uuid/0, x500_uuid/0]).
-export_type([atom_/0, from_string_error/0, uuid/0, version/0, variant/0, format/0, v1_node/0, v1_clock_seq/0]).

-type atom_() :: any().

-type from_string_error() :: atom_not_loaded.

-opaque uuid() :: {uuid, bitstring()}.

-type version() :: v1 | v2 | v3 | v4 | v5 | v_unknown.

-type variant() :: reserved_future | reserved_microsoft | reserved_ncs | rfc4122.

-type format() :: string | hex | urn.

-type v1_node() :: default_node | random_node | {custom_node, binary()}.

-type v1_clock_seq() :: random_clock_seq | {custom_clock_seq, bitstring()}.

-spec v1() -> uuid().
v1() ->
    do_v1(default_uuid1_node(), random_uuid1_clockseq()).

-spec v1_string() -> binary().
v1_string() ->
    _pipe = v1(),
    to_string(_pipe).

-spec v1_custom(v1_node(), v1_clock_seq()) -> {ok, uuid()} | {error, nil}.
v1_custom(Node, Clock_seq) ->
    case {validate_node(Node), validate_clock_seq(Clock_seq)} of
        {{ok, N}, {ok, Cs}} ->
            {ok, do_v1(N, Cs)};

        {_@1, _@2} ->
            {error, nil}
    end.

-spec do_v1(bitstring(), bitstring()) -> uuid().
do_v1(Node, Clock_seq) ->
    <<Node@2:48>> = case Node of
        <<Node@1:48>> -> <<Node@1:48>>;
        _try ->
            erlang:error(#{gleam_error => assert,
                           message => <<"Assertion pattern match failed"/utf8>>,
                           value => _try,
                           module => <<"gleam/uuid"/utf8>>,
                           function => <<"do_v1"/utf8>>,
                           line => 133})
    end,
    <<Time_hi@1:12, Time_mid@1:16, Time_low@1:32>> = case uuid1_time() of
        <<Time_hi:12, Time_mid:16, Time_low:32>> -> <<Time_hi:12,
                                                      Time_mid:16,
                                                      Time_low:32>>;
        _try@1 ->
            erlang:error(#{gleam_error => assert,
                           message => <<"Assertion pattern match failed"/utf8>>,
                           value => _try@1,
                           module => <<"gleam/uuid"/utf8>>,
                           function => <<"do_v1"/utf8>>,
                           line => 134})
    end,
    <<Clock_seq@2:14>> = case Clock_seq of
        <<Clock_seq@1:14>> -> <<Clock_seq@1:14>>;
        _try@2 ->
            erlang:error(#{gleam_error => assert,
                           message => <<"Assertion pattern match failed"/utf8>>,
                           value => _try@2,
                           module => <<"gleam/uuid"/utf8>>,
                           function => <<"do_v1"/utf8>>,
                           line => 135})
    end,
    Value = <<Time_low@1:32,
              Time_mid@1:16,
              1:4,
              Time_hi@1:12,
              2:2,
              Clock_seq@2:14,
              Node@2:48>>,
    {uuid, Value}.

-spec validate_node(v1_node()) -> {ok, bitstring()} | {error, nil}.
validate_node(Node) ->
    case Node of
        default_node ->
            {ok, default_uuid1_node()};

        random_node ->
            {ok, random_uuid1_node()};

        {custom_node, Str} ->
            _pipe = validate_custom_node(
                Str,
                0,
                gleam@bit_builder:from_string(<<""/utf8>>)
            ),
            gleam@result:map(_pipe, fun gleam@bit_builder:to_bit_string/1)
    end.

-spec validate_custom_node(binary(), integer(), gleam@bit_builder:bit_builder()) -> {ok,
                                                                                     gleam@bit_builder:bit_builder()} |
    {error, nil}.
validate_custom_node(Str, Index, Acc) ->
    case gleam@string:pop_grapheme(Str) of
        {error, nil} when Index =:= 12 ->
            {ok, Acc};

        {ok, {<<":"/utf8>>, Rest}} ->
            validate_custom_node(Rest, Index, Acc);

        {ok, {C, Rest@1}} ->
            case hex_to_int(C) of
                {ok, I} when Index < 12 ->
                    validate_custom_node(
                        Rest@1,
                        Index
                        + 1,
                        gleam@bit_builder:append(Acc, <<I:4>>)
                    );

                {error, _@1} ->
                    {error, nil}
            end;

        _@2 ->
            {error, nil}
    end.

-spec validate_clock_seq(v1_clock_seq()) -> {ok, bitstring()} | {error, nil}.
validate_clock_seq(Clock_seq) ->
    case Clock_seq of
        random_clock_seq ->
            {ok, random_uuid1_clockseq()};

        {custom_clock_seq, Bs} ->
            case erlang:bit_size(Bs) =:= 14 of
                true ->
                    {ok, Bs};

                false ->
                    {error, nil}
            end;

        _@1 ->
            {error, nil}
    end.

-spec uuid1_time() -> bitstring().
uuid1_time() ->
    {Mega_sec, Sec, Micro_sec} = os:timestamp(),
    Epoch = ((Mega_sec * 1000000000000) + (Sec * 1000000)) + Micro_sec,
    Timestamp = 122192928000000000 + (10 * Epoch),
    <<Timestamp:60>>.

-spec random_uuid1_clockseq() -> bitstring().
random_uuid1_clockseq() ->
    <<Clock_seq:14, _@1:2>> = crypto:strong_rand_bytes(2),
    <<Clock_seq:14>>.

-spec default_uuid1_node() -> bitstring().
default_uuid1_node() ->
    case inet:getifaddrs() of
        {ok, Ifs} ->
            case find_uuid1_node(Ifs) of
                {ok, Node} ->
                    Node;

                _@1 ->
                    random_uuid1_node()
            end;

        _@2 ->
            random_uuid1_node()
    end.

-spec find_uuid1_node(list({binary(), list({atom_(), list(integer())})})) -> {ok,
                                                                              bitstring()} |
    {error, nil}.
find_uuid1_node(Ifs) ->
    case Ifs of
        [] ->
            {error, nil};

        [{_@1, Props} | Rest] ->
            {ok, Hwaddr@1} = case atom_ffi:atom_from_string(<<"hwaddr"/utf8>>) of
                {ok, Hwaddr} -> {ok, Hwaddr};
                _try ->
                    erlang:error(#{gleam_error => assert,
                                   message => <<"Assertion pattern match failed"/utf8>>,
                                   value => _try,
                                   module => <<"gleam/uuid"/utf8>>,
                                   function => <<"find_uuid1_node"/utf8>>,
                                   line => 227})
            end,
            case gleam@list:key_find(Props, Hwaddr@1) of
                {ok, Ints} ->
                    case (gleam@list:length(Ints)
                    /= 0)
                    orelse gleam@list:all(Ints, fun(X) -> X =:= 0 end) of
                        true ->
                            find_uuid1_node(Rest);

                        false ->
                            {ok, erlang:list_to_binary(Ints)}
                    end;

                _@2 ->
                    find_uuid1_node(Rest)
            end
    end.

-spec random_uuid1_node() -> bitstring().
random_uuid1_node() ->
    <<Rnd_hi@1:7, _@2:1, Rnd_low@1:40>> = case crypto:strong_rand_bytes(6) of
        <<Rnd_hi:7, _@1:1, Rnd_low:40>> -> <<Rnd_hi:7, _@1:1, Rnd_low:40>>;
        _try ->
            erlang:error(#{gleam_error => assert,
                           message => <<"Assertion pattern match failed"/utf8>>,
                           value => _try,
                           module => <<"gleam/uuid"/utf8>>,
                           function => <<"random_uuid1_node"/utf8>>,
                           line => 241})
    end,
    <<Rnd_hi@1:7, 1:1, Rnd_low@1:40>>.

-spec hash_to_uuid_value(bitstring(), integer()) -> bitstring().
hash_to_uuid_value(Hash, Ver) ->
    <<Time_low@1:32,
      Time_mid@1:16,
      _@3:4,
      Time_hi@1:12,
      _@4:2,
      Clock_seq_hi@1:6,
      Clock_seq_low@1:8,
      Node@1:48>> = case Hash of
        <<Time_low:32,
          Time_mid:16,
          _@1:4,
          Time_hi:12,
          _@2:2,
          Clock_seq_hi:6,
          Clock_seq_low:8,
          Node:48>> -> <<Time_low:32,
                         Time_mid:16,
                         _@1:4,
                         Time_hi:12,
                         _@2:2,
                         Clock_seq_hi:6,
                         Clock_seq_low:8,
                         Node:48>>;
        _try ->
            erlang:error(#{gleam_error => assert,
                           message => <<"Assertion pattern match failed"/utf8>>,
                           value => _try,
                           module => <<"gleam/uuid"/utf8>>,
                           function => <<"hash_to_uuid_value"/utf8>>,
                           line => 246})
    end,
    <<Time_low@1:32,
      Time_mid@1:16,
      Ver:4,
      Time_hi@1:12,
      2:2,
      Clock_seq_hi@1:6,
      Clock_seq_low@1:8,
      Node@1:48>>.

-spec v4() -> uuid().
v4() ->
    <<A@1:48, _@3:4, B@1:12, _@4:2, C@1:62>> = case crypto:strong_rand_bytes(16) of
        <<A:48, _@1:4, B:12, _@2:2, C:62>> -> <<A:48, _@1:4, B:12, _@2:2, C:62>>;
        _try ->
            erlang:error(#{gleam_error => assert,
                           message => <<"Assertion pattern match failed"/utf8>>,
                           value => _try,
                           module => <<"gleam/uuid"/utf8>>,
                           function => <<"v4"/utf8>>,
                           line => 274})
    end,
    Value = <<A@1:48, 4:4, B@1:12, 2:2, C@1:62>>,
    {uuid, Value}.

-spec v4_string() -> binary().
v4_string() ->
    _pipe = v4(),
    format(_pipe, string).

-spec v5(uuid(), bitstring()) -> {ok, uuid()} | {error, nil}.
v5(Namespace, Name) ->
    case (erlang:bit_size(Name) rem 8) =:= 0 of
        true ->
            _pipe = erlang:element(2, Namespace),
            _pipe@1 = gleam@bit_builder:from_bit_string(_pipe),
            _pipe@2 = gleam@bit_builder:append(_pipe@1, Name),
            _pipe@3 = gleam@bit_builder:to_bit_string(_pipe@2),
            _pipe@4 = sha1(_pipe@3),
            _pipe@5 = hash_to_uuid_value(_pipe@4, 5),
            _pipe@6 = {uuid, _pipe@5},
            {ok, _pipe@6};

        false ->
            {error, nil}
    end.

-spec sha1(bitstring()) -> bitstring().
sha1(Data) ->
    {ok, Sha@1} = case atom_ffi:atom_from_string(<<"sha"/utf8>>) of
        {ok, Sha} -> {ok, Sha};
        _try ->
            erlang:error(#{gleam_error => assert,
                           message => <<"Assertion pattern match failed"/utf8>>,
                           value => _try,
                           module => <<"gleam/uuid"/utf8>>,
                           function => <<"sha1"/utf8>>,
                           line => 315})
    end,
    <<Sha@3:128, _@2:32>> = case crypto:hash(Sha@1, Data) of
        <<Sha@2:128, _@1:32>> -> <<Sha@2:128, _@1:32>>;
        _try@1 ->
            erlang:error(#{gleam_error => assert,
                           message => <<"Assertion pattern match failed"/utf8>>,
                           value => _try@1,
                           module => <<"gleam/uuid"/utf8>>,
                           function => <<"sha1"/utf8>>,
                           line => 316})
    end,
    <<Sha@3:128>>.

-spec version(uuid()) -> version().
version(Uuid) ->
    <<_@3:48, Ver@1:4, _@4:76>> = case erlang:element(2, Uuid) of
        <<_@1:48, Ver:4, _@2:76>> -> <<_@1:48, Ver:4, _@2:76>>;
        _try ->
            erlang:error(#{gleam_error => assert,
                           message => <<"Assertion pattern match failed"/utf8>>,
                           value => _try,
                           module => <<"gleam/uuid"/utf8>>,
                           function => <<"version"/utf8>>,
                           line => 325})
    end,
    decode_version(Ver@1).

-spec variant(uuid()) -> variant().
variant(Uuid) ->
    <<_@3:64, Var@1:3, _@4:61>> = case erlang:element(2, Uuid) of
        <<_@1:64, Var:3, _@2:61>> -> <<_@1:64, Var:3, _@2:61>>;
        _try ->
            erlang:error(#{gleam_error => assert,
                           message => <<"Assertion pattern match failed"/utf8>>,
                           value => _try,
                           module => <<"gleam/uuid"/utf8>>,
                           function => <<"variant"/utf8>>,
                           line => 331})
    end,
    decode_variant(<<Var@1:3>>).

-spec time(uuid()) -> integer().
time(Uuid) ->
    <<T_low@1:32, T_mid@1:16, _@3:4, T_hi@1:12, _@4:64>> = case erlang:element(
        2,
        Uuid
    ) of
        <<T_low:32, T_mid:16, _@1:4, T_hi:12, _@2:64>> -> <<T_low:32,
                                                            T_mid:16,
                                                            _@1:4,
                                                            T_hi:12,
                                                            _@2:64>>;
        _try ->
            erlang:error(#{gleam_error => assert,
                           message => <<"Assertion pattern match failed"/utf8>>,
                           value => _try,
                           module => <<"gleam/uuid"/utf8>>,
                           function => <<"time"/utf8>>,
                           line => 341})
    end,
    <<T@1:60>> = case <<T_hi@1:12, T_mid@1:16, T_low@1:32>> of
        <<T:60>> -> <<T:60>>;
        _try@1 ->
            erlang:error(#{gleam_error => assert,
                           message => <<"Assertion pattern match failed"/utf8>>,
                           value => _try@1,
                           module => <<"gleam/uuid"/utf8>>,
                           function => <<"time"/utf8>>,
                           line => 342})
    end,
    T@1.

-spec time_posix_microsec(uuid()) -> integer().
time_posix_microsec(Uuid) ->
    case 10 of
        0 -> 0;
        Gleam@denominator -> (time(Uuid) - 122192928000000000) div Gleam@denominator
    end.

-spec clock_sequence(uuid()) -> integer().
clock_sequence(Uuid) ->
    <<_@3:66, Clock_seq@1:14, _@4:48>> = case erlang:element(2, Uuid) of
        <<_@1:66, Clock_seq:14, _@2:48>> -> <<_@1:66, Clock_seq:14, _@2:48>>;
        _try ->
            erlang:error(#{gleam_error => assert,
                           message => <<"Assertion pattern match failed"/utf8>>,
                           value => _try,
                           module => <<"gleam/uuid"/utf8>>,
                           function => <<"clock_sequence"/utf8>>,
                           line => 356})
    end,
    Clock_seq@1.

-spec node(uuid()) -> binary().
node(Uuid) ->
    <<_@2:80,
      A@1:4,
      B@1:4,
      C@1:4,
      D@1:4,
      E@1:4,
      F@1:4,
      G@1:4,
      H@1:4,
      I@1:4,
      J@1:4,
      K@1:4,
      L@1:4>> = case erlang:element(2, Uuid) of
        <<_@1:80, A:4, B:4, C:4, D:4, E:4, F:4, G:4, H:4, I:4, J:4, K:4, L:4>> -> <<_@1:80,
                                                                                    A:4,
                                                                                    B:4,
                                                                                    C:4,
                                                                                    D:4,
                                                                                    E:4,
                                                                                    F:4,
                                                                                    G:4,
                                                                                    H:4,
                                                                                    I:4,
                                                                                    J:4,
                                                                                    K:4,
                                                                                    L:4>>;
        _try ->
            erlang:error(#{gleam_error => assert,
                           message => <<"Assertion pattern match failed"/utf8>>,
                           value => _try,
                           module => <<"gleam/uuid"/utf8>>,
                           function => <<"node"/utf8>>,
                           line => 363})
    end,
    _pipe = [A@1, B@1, C@1, D@1, E@1, F@1, G@1, H@1, I@1, J@1, K@1, L@1],
    _pipe@1 = gleam@list:map(_pipe, fun int_to_hex/1),
    gleam@string:concat(_pipe@1).

-spec to_string(uuid()) -> binary().
to_string(Uuid) ->
    format(Uuid, string).

-spec format(uuid(), format()) -> binary().
format(Uuid, Format) ->
    Separator = case Format of
        string ->
            <<"-"/utf8>>;

        _@1 ->
            <<""/utf8>>
    end,
    Start = case Format of
        urn ->
            [<<"urn:uuid:"/utf8>>];

        _@2 ->
            []
    end,
    to_string_help(erlang:element(2, Uuid), 0, Start, Separator).

-spec to_string_help(bitstring(), integer(), list(binary()), binary()) -> binary().
to_string_help(Ints, Position, Acc, Separator) ->
    case Position of
        8 ->
            to_string_help(Ints, Position + 1, [Separator | Acc], Separator);

        13 ->
            to_string_help(Ints, Position + 1, [Separator | Acc], Separator);

        18 ->
            to_string_help(Ints, Position + 1, [Separator | Acc], Separator);

        23 ->
            to_string_help(Ints, Position + 1, [Separator | Acc], Separator);

        _@1 ->
            case Ints of
                <<I:4, Rest/bitstring>> ->
                    to_string_help(
                        Rest,
                        Position
                        + 1,
                        [int_to_hex(I) | Acc],
                        Separator
                    );

                <<>> ->
                    _pipe = Acc,
                    _pipe@1 = gleam@list:reverse(_pipe),
                    gleam@string:concat(_pipe@1)
            end
    end.

-spec from_string(binary()) -> {ok, uuid()} | {error, nil}.
from_string(In) ->
    Hex = case gleam@string:starts_with(In, <<"urn:uuid:"/utf8>>) of
        true ->
            gleam@string:drop_left(In, 9);

        false ->
            In
    end,
    case to_bitstring(Hex) of
        {ok, Bits} ->
            {ok, {uuid, Bits}};

        {error, _@1} ->
            {error, nil}
    end.

-spec dns_uuid() -> uuid().
dns_uuid() ->
    {uuid, <<143098242404177361603877621312831893704:128>>}.

-spec url_uuid() -> uuid().
url_uuid() ->
    {uuid, <<143098242483405524118141958906375844040:128>>}.

-spec oid_uuid() -> uuid().
oid_uuid() ->
    {uuid, <<143098242562633686632406296499919794376:128>>}.

-spec x500_uuid() -> uuid().
x500_uuid() ->
    {uuid, <<143098242721090011660934971687007695048:128>>}.

-spec to_bitstring(binary()) -> {ok, bitstring()} | {error, nil}.
to_bitstring(Str) ->
    _pipe = Str,
    _pipe@1 = to_bitstring_help(
        _pipe,
        0,
        gleam@bit_builder:from_string(<<""/utf8>>)
    ),
    gleam@result:map(_pipe@1, fun gleam@bit_builder:to_bit_string/1).

-spec to_bitstring_help(binary(), integer(), gleam@bit_builder:bit_builder()) -> {ok,
                                                                                  gleam@bit_builder:bit_builder()} |
    {error, nil}.
to_bitstring_help(Str, Index, Acc) ->
    case gleam@string:pop_grapheme(Str) of
        {error, nil} when Index =:= 32 ->
            {ok, Acc};

        {ok, {<<"-"/utf8>>, Rest}} when Index < 32 ->
            to_bitstring_help(Rest, Index, Acc);

        {ok, {C, Rest@1}} when Index < 32 ->
            case hex_to_int(C) of
                {ok, I} ->
                    to_bitstring_help(
                        Rest@1,
                        Index
                        + 1,
                        gleam@bit_builder:append(Acc, <<I:4>>)
                    );

                {error, _@1} ->
                    {error, nil}
            end;

        _@2 ->
            {error, nil}
    end.

-spec decode_version(integer()) -> version().
decode_version(Int) ->
    case Int of
        1 ->
            v1;

        2 ->
            v2;

        3 ->
            v3;

        4 ->
            v4;

        5 ->
            v5;

        _@1 ->
            v_unknown
    end.

-spec decode_variant(bitstring()) -> variant().
decode_variant(Variant_bits) ->
    case Variant_bits of
        <<1:1, 1:1, 1:1>> ->
            reserved_future;

        <<1:1, 1:1, 0:1>> ->
            reserved_microsoft;

        <<1:1, 0:1, _@1:1>> ->
            rfc4122;

        <<0:1, _@2:1, _@3:1>> ->
            reserved_ncs
    end.

-spec hex_to_int(binary()) -> {ok, integer()} | {error, nil}.
hex_to_int(C) ->
    I = case C of
        <<"0"/utf8>> ->
            0;

        <<"1"/utf8>> ->
            1;

        <<"2"/utf8>> ->
            2;

        <<"3"/utf8>> ->
            3;

        <<"4"/utf8>> ->
            4;

        <<"5"/utf8>> ->
            5;

        <<"6"/utf8>> ->
            6;

        <<"7"/utf8>> ->
            7;

        <<"8"/utf8>> ->
            8;

        <<"9"/utf8>> ->
            9;

        <<"a"/utf8>> ->
            10;

        <<"A"/utf8>> ->
            10;

        <<"b"/utf8>> ->
            11;

        <<"B"/utf8>> ->
            11;

        <<"c"/utf8>> ->
            12;

        <<"C"/utf8>> ->
            12;

        <<"d"/utf8>> ->
            13;

        <<"D"/utf8>> ->
            13;

        <<"e"/utf8>> ->
            14;

        <<"E"/utf8>> ->
            14;

        <<"f"/utf8>> ->
            15;

        <<"F"/utf8>> ->
            15;

        _@1 ->
            16
    end,
    case I of
        16 ->
            {error, nil};

        X ->
            {ok, X}
    end.

-spec int_to_hex(integer()) -> binary().
int_to_hex(Int) ->
    erlang:integer_to_binary(Int, 16).
