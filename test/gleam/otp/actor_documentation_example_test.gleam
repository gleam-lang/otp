import gleam/erlang/process.{Subject}
import gleam/otp/actor.{StartError}

pub fn example_test() {
  // Start the actor. We assert that it starts successfully.
  let assert Ok(actor) = start_stack_actor()

  // We can send a message to the actor to push elements onto the stack.
  //
  // In real-world Gleam OTP programs we would likely write a wrapper functions
  // called `push` and `pop` to send these messages. We are not doing that here
  // to show how the message sending functions are used.
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
  let assert Error(StackWasEmpty) = process.call(actor, Pop, 10)

  // Lastly, we can send a message to the actor asking it to shut down.
  process.send(actor, Shutdown)
}

pub type Message(element) {
  Push(push: element)

  Pop(reply_with: Subject(Result(element, PopError)))

  Shutdown
}

pub type PopError {
  StackWasEmpty
}

fn start_stack_actor() -> Result(Subject(Message(element)), StartError) {
  let initial_state = []
  actor.start(initial_state, handle_message)
}

fn handle_message(message: Message(e), stack: List(e)) -> actor.Next(List(e)) {
  case message {
    Shutdown -> actor.Stop(process.Normal)

    Push(value) -> actor.Continue([value, ..stack])

    Pop(client) ->
      case stack {
        [] -> {
          process.send(client, Error(StackWasEmpty))
          actor.Continue([])
        }

        [first, ..rest] -> {
          process.send(client, Ok(first))
          actor.Continue(rest)
        }
      }
  }
}
