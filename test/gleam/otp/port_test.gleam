import gleam/dynamic
import gleam/otp/port
import gleeunit/should

type PortName {
  Spawn(String)
}

type PortSetting

pub type PortMsg {
  Port(port: port.Port)
}

@external(erlang, "erlang", "open_port")
fn open_port(
  port_name: PortName,
  port_settings: List(PortSetting),
) -> dynamic.Dynamic

pub fn port_dynamic_test() {
  let msg = open_port(Spawn("echo \"hello world\""), [])

  msg
  |> dynamic.decode1(Port, port.port_from_dynamic)
  |> should.be_ok()

  dynamic.from("")
  |> dynamic.decode1(Port, port.port_from_dynamic)
  |> should.be_error()
}
