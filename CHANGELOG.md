# Changelog

## Unreleased

- Fixed an issue where the `gleam/otp/process.bare_message_receiver` function ignored messages from ports

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
