# Changelog

## Unreleased

- Add `Selecting` constructor to `actor.Next` allowing for replacement of an actor's selector from the loop function.

## v0.6.0 - 2023-07-27

- Updated for Gleam v0.30.

## v0.5.3 - 2023-03-30

- Updated for Gleam v0.27.

## v0.5.2 - 2022-11-15

- Updated for latest `gleam_erlang` and Gleam versions.

## v0.5.1 - 2022-11-11

- Fixed bug: tasks are now unmonitored by their parent after completion.

## v0.5.0 - 2022-08-09

- Updated to use the process module in the `gleam_erlang` library`.

## v0.4.0 - 2022-04-16

- The `gleam/otp/process` module gains the `pid_from_dynamic` function.
- The `gleam/otp/task` module gains the `await_forever` and `try_await_forever` functions.

## v0.3.1 - 2022-01-07

- Updated for Gleam v0.19.0.

## v0.3.0 - 2021-11-23

- Converted to use the Gleam build tool.

## v0.2.0 - 2021-09-11

- Updated for Gleam v0.17.0.

## v0.1.6 - 2021-08-02

- Updated to use the new `#()` tuple syntax.

## v0.1.5 - 2021-06-01

- Warnings fixed for current versions of Gleam.

## v0.1.4 - 2020-12-31

- Port messages are no longer treated as regular channel messages by
  receivers.

## v0.1.3 - 2020-11-07

- The `gleam/otp/actor.ErlangStartResult` type has been re-added.
- The `gleam/otp/actor` module now re-exports the `pid` function of the
  `gleam/otp/process` module.

## v0.1.2 - 2020-11-02

- The `gleam/otp/actor.ErlangStartResult` type has been removed.

## v0.1.1 - 2020-10-31

- The `gleam/otp/process.Sender` type is now opaque.

## v0.1.0 - 2020-10-31

- Initial minimal release.
