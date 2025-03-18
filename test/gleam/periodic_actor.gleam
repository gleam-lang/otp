//// An example of an actor that concurrently does something every X amount of
//// time (or when it is sent a message by another process).

import gleam/erlang/process
import gleam/io
import gleam/otp/actor.{type StartError}

pub fn periodic_actor(
  every period_milliseconds: Int,
  run callback: fn() -> Nil,
) -> Result(actor.Started(Nil), StartError) {
  let init = fn(subject) {
    let selector =
      process.new_selector()
      |> process.selecting(subject, fn(x) { x })
    // Send the first message to trigger the looping
    process.send(subject, Nil)
    // We're ready to start receiving messages
    actor.initialised(subject)
    |> actor.selecting(selector)
    |> Ok
  }

  let loop = fn(subject, _msg) {
    // Send a message to itself in the future
    process.send_after(subject, period_milliseconds, Nil)
    // Run the callback as the timer has triggered again
    callback()
    // We're done, await the next message
    actor.continue(subject)
  }

  // Start the actor
  actor.new_with_initialiser(50, init)
  |> actor.on_message(loop)
  |> actor.start
}

pub fn main_test() {
  let five_seconds = 5000
  let say_hello = fn() { io.println("Hello, Joe!") }
  periodic_actor(every: five_seconds, run: say_hello)
}
