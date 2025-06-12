# Gleam OTP

<a href="https://github.com/gleam-lang/otp/releases"><img src="https://img.shields.io/github/release/gleam-lang/otp" alt="GitHub release"></a>
<a href="https://discord.gg/Fm8Pwmy"><img src="https://img.shields.io/discord/768594524158427167?color=blue" alt="Discord chat"></a>
![CI](https://github.com/gleam-lang/otp/workflows/test/badge.svg?branch=main)

A Gleam library for building fault tolerant multi-core programs using the
actor model. It is compatible with Erlang's OTP framework.

Gleam’s actor system is built with a few primary goals:

- Full type safety of actors and messages.
- Be compatible with Erlang’s OTP actor framework.
- Provide fault tolerance and self-healing through supervisors.
- Have equivalent performance to Erlang’s OTP.

This library documents its abstractions and functionality, but you may also wish
to read the documentation or other material on Erlang’s OTP framework to get a
fuller understanding of OTP, the problems it solves, and and the motivations for
its design.

Not all Erlang/OTP functionality is included in this library. Some is not
possible to represent in a type safe way, so it is not included. Other features
are still in development, such as further process supervision strategies.

## Usage

Add this library to your Gleam project.

```shell
gleam add gleam_otp
```

## Common types of actor

This library provides several different types of actor that can be used in
Gleam programs.

### Process

The process is the lowest level building block of OTP, all other actors are
built on top of processes either directly or indirectly. Typically this
abstraction would not be used very often in Gleam applications, favour
other actor types that provide more functionality.

Gleam's [process](https://hexdocs.pm/gleam_erlang/gleam/erlang/process.html) module is defined in the `gleam_erlang` library.

### Actor

The `actor` is the most commonly used process type in Gleam and serves as a good
building block for other abstractions. Like Erlang's `gen_server` it handles
OTP's system messages automatically to enable OTP's debugging and tracing
functionality.

[Documentation](https://hexdocs.pm/gleam_otp/gleam/otp/actor.html)

### Supervisor

Supervisors is a process that starts and then supervises a group of processes,
restarting them if they crash. Supervisors can start other supervisors,
resulting in a hierarchical process structure called a supervision tree,
providing fault tolerance to a Gleam application.

- [gleam/otp/static_supervisor](https://hexdocs.pm/gleam_otp/gleam/otp/static_supervisor.html) documentation.
  documentation.

## Limitations and known issues

Actors do not yet support all OTP system messages, so some of the OTP debugging
APIs may not be fully functional. These unsupported messages are discarded by
actors.
