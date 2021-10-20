import gleam/uuid
import gleam/should
import gleam/result

pub fn v1_from_string_test() {
  "49cac37c-310b-11eb-adc1-0242ac120002"
  |> uuid.from_string()
  |> result.map(uuid.version)
  |> should.equal(Ok(uuid.V1))
}

pub fn v4_from_string_test() {
  "16b53fc5-f9a7-4f6b-8180-399ab0986250"
  |> uuid.from_string()
  |> result.map(uuid.version)
  |> should.equal(Ok(uuid.V4))
}

pub fn unknown_version_test() {
  "16b53fc5-f9a7-0f6b-8180-399ab0986250"
  |> uuid.from_string()
  |> result.map(uuid.version)
  |> should.equal(Ok(uuid.VUnknown))
}

pub fn too_short_test() {
  "16b53fc5-f9a7-4f6b-8180-399ab098625"
  |> uuid.from_string()
  |> should.equal(Error(Nil))
}

pub fn too_long_test() {
  "16b53fc5-f9a7-4f6b-8180-399ab09862500"
  |> uuid.from_string()
  |> should.equal(Error(Nil))
}

pub fn non_hex_char_test() {
  "16z53fc5-f9a7-4f6b-8180-399ab0986250"
  |> uuid.from_string()
  |> should.equal(Error(Nil))
}

//
// V1 Tests
//
pub fn v1_can_validate_self_test() {
  let uuid =
    uuid.v1()
    |> uuid.to_string()
    |> uuid.from_string()

  uuid
  |> result.map(uuid.version)
  |> should.equal(Ok(uuid.V1))

  uuid
  |> result.map(uuid.variant)
  |> should.equal(Ok(uuid.Rfc4122))
}

pub fn v1_custom_node_and_clock_seq() {
  let node = "B6:00:CD:CA:75:C7"
  let node_no_colons = "B600CDCA75C7"
  let clock_seq = 15000
  let uuid =
    uuid.v1_custom(uuid.CustomNode(node), uuid.CustomClockSeq(<<clock_seq:14>>))

  uuid
  |> result.map(uuid.node)
  |> should.equal(Ok(node_no_colons))

  uuid
  |> result.map(uuid.clock_sequence)
  |> should.equal(Ok(clock_seq))
}

//
// V4 Tests
//
pub fn v4_can_validate_self_test() {
  let uuid =
    uuid.v4()
    |> uuid.to_string()
    |> uuid.from_string()

  uuid
  |> result.map(uuid.version)
  |> should.equal(Ok(uuid.V4))

  uuid
  |> result.map(uuid.variant)
  |> should.equal(Ok(uuid.Rfc4122))
}

//
// V5 Tests
//
pub fn v5_dns_namespace_test() {
  uuid.v5(uuid.dns_uuid(), <<"my.domain.com":utf8>>)
  |> result.map(uuid.to_string)
  |> should.equal(Ok("016C25FD-70E0-56FE-9D1A-56E80FA20B82"))
}

pub fn v5_dont_crash_on_bad_name_test() {
  uuid.v5(uuid.dns_uuid(), <<1:1>>)
  |> should.equal(Error(Nil))
}
