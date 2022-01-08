//// An example of an actor that concurrently does something every X amount of
//// time (or when it is sent a message by another process).

import gleam/otp/process.{Sender}
import gleam/otp/actor.{Continue, Ready, Spec, StartError}
import gleam/option.{Some}
import gleam/io

pub fn periodic_actor(
  every period_milliseconds: Int,
  run callback: fn() -> Nil,
) -> Result(Sender(Nil), StartError) {
  let init = fn() {
    // Create a channel to periodically send a message to the actor on
    let #(sender, receiver) = process.new_channel()
    // Send the first message to trigger the looping
    process.send(sender, Nil)
    // We're ready to start receiving messages
    Ready(sender, Some(receiver))
  }

  let loop = fn(_msg, sender) {
    // Send a message to itself in the future
    process.send_after(sender, period_milliseconds, Nil)
    // Run the callback as the timer has triggered again
    callback()
    // We're done, await the next message
    Continue(sender)
  }

  // Start the actor
  actor.start_spec(Spec(init: init, loop: loop, init_timeout: 50))
}

pub fn main_test() {
  let second = 1000
  let say_hello = fn() { io.println("Hello, Joe!") }
  periodic_actor(every: second, run: say_hello)
}
