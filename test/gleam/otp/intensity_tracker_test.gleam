import gleam/erlang/process
import gleam/otp/intensity_tracker.{type IntensityTracker, add_event, new}
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

pub fn errors_when_too_intense_test() {
  let count = 5
  new(limit: count, period: 1)
  |> add_events(count, _)
  |> add_event()
  |> should.be_error()
}

pub fn cools_down_after_period_test() {
  let count = 1000
  let limiter = new(limit: count, period: 1)
  |> add_events(count, _)

  // Since the intensity_tracker is enforced in seconds,
  // wait for that second to pass
  process.sleep(1000)
  limiter
  |> add_events(count, _)
}

fn add_events(count: Int, limiter: IntensityTracker) {
  case count {
    0 -> limiter
    i if i > 0 -> {
      limiter
      |> add_event()
      |> should.be_ok()
      |> add_events(count - 1, _)
    }
    _ -> panic
  }
}
