-module(gleam_uuid_test).
-compile(no_auto_import).

-export([v1_from_string_test/0, v4_from_string_test/0, unknown_version_test/0, too_short_test/0, too_long_test/0, non_hex_char_test/0, v1_can_validate_self_test/0, v1_custom_node_and_clock_seq/0, v4_can_validate_self_test/0, v5_dns_namespace_test/0, v5_dont_crash_on_bad_name_test/0]).

-spec v1_from_string_test() -> nil.
v1_from_string_test() ->
    _pipe = <<"49cac37c-310b-11eb-adc1-0242ac120002"/utf8>>,
    _pipe@1 = gleam@uuid:from_string(_pipe),
    _pipe@2 = gleam@result:map(_pipe@1, fun gleam@uuid:version/1),
    gleam@should:equal(_pipe@2, {ok, v1}).

-spec v4_from_string_test() -> nil.
v4_from_string_test() ->
    _pipe = <<"16b53fc5-f9a7-4f6b-8180-399ab0986250"/utf8>>,
    _pipe@1 = gleam@uuid:from_string(_pipe),
    _pipe@2 = gleam@result:map(_pipe@1, fun gleam@uuid:version/1),
    gleam@should:equal(_pipe@2, {ok, v4}).

-spec unknown_version_test() -> nil.
unknown_version_test() ->
    _pipe = <<"16b53fc5-f9a7-0f6b-8180-399ab0986250"/utf8>>,
    _pipe@1 = gleam@uuid:from_string(_pipe),
    _pipe@2 = gleam@result:map(_pipe@1, fun gleam@uuid:version/1),
    gleam@should:equal(_pipe@2, {ok, v_unknown}).

-spec too_short_test() -> nil.
too_short_test() ->
    _pipe = <<"16b53fc5-f9a7-4f6b-8180-399ab098625"/utf8>>,
    _pipe@1 = gleam@uuid:from_string(_pipe),
    gleam@should:equal(_pipe@1, {error, nil}).

-spec too_long_test() -> nil.
too_long_test() ->
    _pipe = <<"16b53fc5-f9a7-4f6b-8180-399ab09862500"/utf8>>,
    _pipe@1 = gleam@uuid:from_string(_pipe),
    gleam@should:equal(_pipe@1, {error, nil}).

-spec non_hex_char_test() -> nil.
non_hex_char_test() ->
    _pipe = <<"16z53fc5-f9a7-4f6b-8180-399ab0986250"/utf8>>,
    _pipe@1 = gleam@uuid:from_string(_pipe),
    gleam@should:equal(_pipe@1, {error, nil}).

-spec v1_can_validate_self_test() -> nil.
v1_can_validate_self_test() ->
    Uuid = begin
        _pipe = gleam@uuid:v1(),
        _pipe@1 = gleam@uuid:to_string(_pipe),
        gleam@uuid:from_string(_pipe@1)
    end,
    _pipe@2 = Uuid,
    _pipe@3 = gleam@result:map(_pipe@2, fun gleam@uuid:version/1),
    gleam@should:equal(_pipe@3, {ok, v1}),
    _pipe@4 = Uuid,
    _pipe@5 = gleam@result:map(_pipe@4, fun gleam@uuid:variant/1),
    gleam@should:equal(_pipe@5, {ok, rfc4122}).

-spec v1_custom_node_and_clock_seq() -> nil.
v1_custom_node_and_clock_seq() ->
    Node = <<"B6:00:CD:CA:75:C7"/utf8>>,
    Node_no_colons = <<"B600CDCA75C7"/utf8>>,
    Clock_seq = 15000,
    Uuid = gleam@uuid:v1_custom(
        {custom_node, Node},
        {custom_clock_seq, <<Clock_seq:14>>}
    ),
    _pipe = Uuid,
    _pipe@1 = gleam@result:map(_pipe, fun gleam@uuid:node/1),
    gleam@should:equal(_pipe@1, {ok, Node_no_colons}),
    _pipe@2 = Uuid,
    _pipe@3 = gleam@result:map(_pipe@2, fun gleam@uuid:clock_sequence/1),
    gleam@should:equal(_pipe@3, {ok, Clock_seq}).

-spec v4_can_validate_self_test() -> nil.
v4_can_validate_self_test() ->
    Uuid = begin
        _pipe = gleam@uuid:v4(),
        _pipe@1 = gleam@uuid:to_string(_pipe),
        gleam@uuid:from_string(_pipe@1)
    end,
    _pipe@2 = Uuid,
    _pipe@3 = gleam@result:map(_pipe@2, fun gleam@uuid:version/1),
    gleam@should:equal(_pipe@3, {ok, v4}),
    _pipe@4 = Uuid,
    _pipe@5 = gleam@result:map(_pipe@4, fun gleam@uuid:variant/1),
    gleam@should:equal(_pipe@5, {ok, rfc4122}).

-spec v5_dns_namespace_test() -> nil.
v5_dns_namespace_test() ->
    _pipe = gleam@uuid:v5(gleam@uuid:dns_uuid(), <<"my.domain.com"/utf8>>),
    _pipe@1 = gleam@result:map(_pipe, fun gleam@uuid:to_string/1),
    gleam@should:equal(
        _pipe@1,
        {ok, <<"016C25FD-70E0-56FE-9D1A-56E80FA20B82"/utf8>>}
    ).

-spec v5_dont_crash_on_bad_name_test() -> nil.
v5_dont_crash_on_bad_name_test() ->
    _pipe = gleam@uuid:v5(gleam@uuid:dns_uuid(), <<1:1>>),
    gleam@should:equal(_pipe, {error, nil}).
