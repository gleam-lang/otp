# gleam_otp_process

A Gleam library for working with Erlang/OTP processes.

The process abstraction here is a very thin layer over Erlang and as such is
is opaque and provides little to no type safety. This is useful for interop
with Erlang systems but type safe abstractions should be used where possible.


## Quick start

```sh
# Build the project
rebar3 compile

# Run the eunit tests
rebar3 eunit

# Run the Erlang REPL
rebar3 shell
```
