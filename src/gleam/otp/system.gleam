import gleam/dynamic.{Dynamic}
import gleam/otp/process.{Pid}

pub external fn get_state(from: Pid(a)) -> Dynamic =
  "sys" "get_state"
