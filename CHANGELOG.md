# Changelog

## v1.0.0-rc1 - Unreleased

- The `supervisor` module has been removed.
- The `intensity_tracker` module has been removed.
- The `task` module has been removed.
- The supervision module has been introduced. This module contains types and
  functions usable by different supervisor implementations.
- In the `gleam/otp/static_supervisor` module:
  - The `start_link` module has been removed.
  - The `start` module has been added.
  - Types and functions for defining child specifications have been moved to the
    `supervision` module.
  - Child ids are now generated, removing possibility of a collision.
  - The `SupervisorHandle` type has been added.
- In the `gleam/otp/actor` module:
  - The `to_erlang_start_result` function has been removed.
  - The `InitResult` type has been removed.
  - The `Spec` type has been replaced with the `Builder` type.
  - The `new` function has been added.
  - The `new_with_initialiser` function has been added.
  - The `start` function has been removed.
  - The `start_spec` function has been renamed to `start`.
  - The `Initialised` type has been added, along with the `initialised`,
    `selecting`, and `returning` functions to create and work with it.
  - The `InitCrashed` variant of `StartError` has been replaced with the
    `InitExited` variant.
  - The `InitFailed` variant of `StartError` now contains a string rather than
    an `StartError`.
  - The `Next` type now has the state type as the first type parameter and the
    message type as the second.
  - The argument order of the actor message handler callback function has been
    changed so the state is the first argument, and the message is the second.
  - The argument order of `call` has changed.
  - The `StartResult` alias has been removed.
  - The `ErlangStartResult` alias has been removed.

## Unreleased

- Fixed a bug where using `actor.Stop()` with other reasons than `Normal`
  would stop the process with `Normal`.

## v0.16.1 - 2025-01-20

- Fixed a bug where the static supervisor would return the incorrect value if
  it crashes when initialising.

## v0.16.0 - 2024-12-10

- Fixed a bug where if an actor times out when starting it would not unlink from
  the parent.

## v0.15.0 - 2024-12-08

- The deprecated `try_await_forever` function in the `gleam/otp/task` module has
  been removed.

## v0.14.1 - 2024-11-15

- Fixed a bug where the `significant` parameter would not be passed to the
  supervisor in `static_supervisor`.

## v0.14.0 - 2024-11-10

- The `gleam/otp/task` gains the `try_await_all` function.

## v0.13.1 - 2024-10-31

- The `try_await_forever` function in the `gleam/otp/task` module has been
  deprecated.
- The `gleam/otp/task` module gains the `pid`, `await2`, `await3`, and `await4`
  functions.
- Tasks are no longer monitored.

## v0.12.1 - 2024-09-30

- The minimum required Gleam version in `gleam.toml` has been increased to match
  the actual requirement for this package.

## v0.12.0 - 2024-09-03

- `Port` type has been deprecated in favour of the `gleam_erlang`
  package `Port` type.

## v0.11.2 - 2024-08-21

- Fixed support for older versions of Gleam.

## v0.11.1 - 2024-08-19

- Fixed support for older versions of Gleam.

## v0.11.0 - 2024-08-16

- The `gleam/otp/static_supervisor` module has been added, containing bindings
  to Erlang/OTP's supervisor module.

## v0.10.0 - 2024-03-11

- Fixed bug: supervisors now abort restarting children if limits are exceeded.
- Fixed `gleam/otp/system.get_state/1` calls that break in Erlang/OTP >= 26.1.
  `get_state/1` (used in debugging and tests) will error on Erlang/OTP <=
  26.0 with "No case clause matched".

## v0.9.0 - 2024-01-03

- The useless `gleam_otp` module has been removed.

## v0.8.0 - 2023-11-06

- Updated for Gleam v0.32.0.
- Version requirement for `gleam_erlang` updated to `~> 0.22`.

## v0.7.0 - 2023-08-30

- The `actor.Continue` record now contains an optional `Selector`, which can be
  used to change the messages the actor is selecting. The `continue` and
  `with_selector` builder functions have been added to aid with construction of
  `Next` values.
- The `gleam/otp/node` module has been removed in favour of the
  `gleam/erlang/node` module in the `gleam_erlang` package.

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
- The `gleam/otp/task` module gains the `await_forever` and `try_await_forever`
  functions.

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
