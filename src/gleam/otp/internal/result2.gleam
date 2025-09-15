/// An alernative ok type used within Erlang/OTP
pub type Result2(data1, data2, error) {
  Ok(data1, data2)
  Error(error)
}
