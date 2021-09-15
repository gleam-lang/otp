# Gleam OTP

<a href="https://github.com/gleam-lang/otp/releases"><img src="https://img.shields.io/github/release/gleam-lang/otp" alt="GitHub release"></a>
<a href="https://discord.gg/Fm8Pwmy"><img src="https://img.shields.io/discord/768594524158427167?color=blue" alt="Discord chat"></a>
![CI](https://github.com/gleam-lang/otp/workflows/test/badge.svg?branch=master)

A Gleam library for building fault tolerant multi-core programs using the
actor model. It is compatible with Erlang's OTP framework.

This library is experimental and will likely have many breaking changes in the
future!

Gleam’s actor system is built with a few primary goals:

 - Full type safety of actors and messages.
 - Be compatible with Erlang’s OTP actor framework.
 - Provide fault tolerance and self-healing through supervisors.
 - Have equivalent performance to Erlang’s OTP.

## Getting started

### Getting OTP onto your system

To use the OTP library in a Gleam project, add `gleam_otp` to the `deps` section of the projects `rebar.config` file. The result might, for instance, look like this:
```
  {deps, [
    {gleam_stdlib, "0.16.0"},
    {gleam_otp, "0.1.6"}
  ]}.
```
You might have to futz with the version number to match other components of your system.

Then run `rebar3 get-deps`.

The next time you run `rebar3 shell` (or any other rebar3 command that induces compilation), rebar3 should download the deps as you've specified. If it doesn't, you might want to delete the `rebar.lock` file in the project root, if it's present.

### How to understand the Gleam OTP library

(1) Read the rest of this README.

(2) Understand Erlang's OTP library.

(3) this [blog post](https://gleam.run/news/gleam-v0.12-and-gleam-otp-v0.1-released/) is a decent jumping off point.

(4) [Gleam OTP test suite](https://github.com/gleam-lang/otp/tree/main/test/gleam/otp) demonstrates what the library offers in more detail.

## Actor hierarchy

This library defines several different types of actor that can be used in
Gleam programs.

```
      Process
      ↙    ↘
   Actor   Task
     ↓
Supervisor
```

### Process

The process is the lowest level building block of OTP, all other actors are
built on top of processes either directly or indirectly. Typically this
abstraction would be not be used very often in Gleam applications, favour
other actor types that provide more functionality.

### Actor

The `actor` is the most commonly used process type in Gleam and serves as a good
building block for other abstractions. Like Erlang's `gen_server` it will
automatically handle OTP's debug system messages for you.

### Task

A task is a kind of process that performs a single task and then shuts down.
Commonly tasks are used to convert sequential code into concurrent code by
performing computation in another process.

### Supervisor

Supervisors is a process that starts and then supervises a group of processes,
restarting them if they crash. Supervisors can start other supervisors,
resulting in a hierarchical process structure called a supervision tree,
providing fault tolerance to a Gleam application.

## Limitations and known issues

This library is experimental there are some limitations that not yet been resolved.

- There is no support for named processes.
- Actors do not yet support all OTP system messages. Unsupported messages are dropped.
- Supervisors do not yet support different shutdown periods per child. In
  practice this means that children that are supervisors do not get an
  unlimited amount of time to shut down, as is expected in Erlang or Elixir.
- This library has not seen much testing compared to the Erlang OTP
  libraries, both in terms of unit tests and real world testing in
  applications.
