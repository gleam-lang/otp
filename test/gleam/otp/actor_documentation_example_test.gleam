import gleam/erlang/process.{Subject}
import gleam/otp/actor

pub fn example_test() {
  // Start the actor with initial state of an empty list, and the
  // `handle_message` callback function (defined below).
  // We assert that it starts successfully.
  // 
  // In real-world Gleam OTP programs we would likely write a wrapper functions
  // called `start`, `push` `pop`, `shutdown` to start and interact with the
  // Actor. We are not doing that here for the sake of showing how the Actor 
  // API works.
  let assert Ok(actor) = actor.start([], handle_message)

  // We can send a message to the actor to push elements onto the stack.
  process.send(actor, Push("Joe"))
  process.send(actor, Push("Mike"))
  process.send(actor, Push("Robert"))

  // The `Push` message expects no response, these messages are sent purely for
  // the side effect of mutating the state held by the actor.
  //
  // We can also send the `Pop` message to take a value off of the actor's
  // stack. This message expects a response, so we use `process.call` to send a
  // message and wait until a reply is received.
  //
  // In this instance we are giving the actor 10 milliseconds to reply, if the
  // `call` function doesn't get a reply within this time it will panic and
  // crash the client process.
  let assert Ok("Robert") = process.call(actor, Pop, 10)
  let assert Ok("Mike") = process.call(actor, Pop, 10)
  let assert Ok("Joe") = process.call(actor, Pop, 10)

  // The stack is now empty, so if we pop again the actor replies with an error.
  let assert Error(Nil) = process.call(actor, Pop, 10)

  // Lastly, we can send a message to the actor asking it to shut down.
  process.send(actor, Shutdown)
}

pub type Message(element) {
  Push(push: element)

  Pop(reply_with: Subject(Result(element, Nil)))

  Shutdown
}

fn handle_message(message: Message(e), stack: List(e)) -> actor.Next(List(e)) {
  case message {
    Shutdown -> actor.Stop(process.Normal)

    Push(value) -> actor.Continue([value, ..stack])

    Pop(client) ->
      case stack {
        [] -> {
          process.send(client, Error(Nil))
          actor.Continue([])
        }

        [first, ..rest] -> {
          process.send(client, Ok(first))
          actor.Continue(rest)
        }
      }
  }
}
