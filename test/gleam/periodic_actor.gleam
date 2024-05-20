//// An example of an actor that concurrently does something every X amount of
//// time (or when it is sent a message by another process).

import gleam/erlang/process.{type Subject}
import gleam/io
import gleam/otp/actor.{type StartError, Ready, Spec}

pub fn periodic_actor(
  every period_milliseconds: Int,
  run callback: fn() -> Nil,
) -> Result(Subject(Nil), StartError) {
  let init = fn(
    // Create a channel to periodically send a message to the actor on
  ) {
    let subject = process.new_subject()
    let selector =
      process.new_selector()
      |> process.selecting(subject, fn(x) { x })
    // Send the first message to trigger the looping
    process.send(subject, Nil)
    // We're ready to start receiving messages
    Ready(subject, selector)
  }

  let loop = fn(
    _msg,
    subject,
    // Send a message to itself in the future
  ) {
    process.send_after(subject, period_milliseconds, Nil)
    // Run the callback as the timer has triggered again
    callback()
    // We're done, await the next message
    actor.continue(subject)
  }

  // Start the actor
  actor.start_spec(Spec(init: init, loop: loop, init_timeout: 50))
}

pub fn main_test() {
  let five_seconds = 5000
  let say_hello = fn() { io.println("Hello, Joe!") }
  periodic_actor(every: five_seconds, run: say_hello)
}
