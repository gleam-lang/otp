import gleam/otp/node.{Node}

pub type StartType {
  Normal
  Takeover(Node)
  Failover(Node)
}
