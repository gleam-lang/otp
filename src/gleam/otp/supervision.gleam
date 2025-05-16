import gleam/otp/actor

/// Restart defines when a terminated child process must be restarted. 
pub type Restart {
  /// A permanent child process is always restarted.
  Permanent
  /// A transient child process is restarted only if it terminates abnormally,
  /// that is, with another exit reason than `normal`, `shutdown`, or
  /// `{shutdown,Term}`.
  Transient
  /// A temporary child process is never restarted (even when the supervisor's
  /// restart strategy is `RestForOne` or `OneForAll` and a sibling's death
  /// causes the temporary process to be terminated).
  Temporary
}

pub type ChildType {
  /// A worker child has to shut-down within a given amount of time.
  Worker(
    /// The number of milliseconds the child is given to shut down. The
    /// supervisor tells the child process to terminate by calling
    /// `exit(Child,shutdown)` and then wait for an exit signal with reason
    /// shutdown back from the child process. If no exit signal is received
    /// within the specified number of milliseconds, the child process is
    /// unconditionally terminated using `exit(Child,kill)`.
    shutdown_ms: Int,
  )
  Supervisor
}

/// A description a how to start a new child process under an OTP supervisor.
pub type ChildSpecification(data) {
  ChildSpecification(
    /// A function to call to start the child process.
    start: fn() -> Result(actor.Started(data), actor.StartError),
    /// When the child is to be restarted. See the `Restart` documentation for
    /// more.
    ///
    /// You most likely want the `Permanent` variant.
    restart: Restart,
    /// This defines if a child is considered significant for automatic
    /// self-shutdown of the supervisor.
    ///
    /// You most likely do not want to consider any children significant.
    ///
    /// This will be ignored if the supervisor auto shutdown is set to `Never`,
    /// which is the default.
    significant: Bool,
    /// Whether the child is a supervisor or not.
    child_type: ChildType,
  )
}

/// A regular child process.
///
/// You should use this unless your process is also a supervisor.
///
pub fn worker(
  run start: fn() -> Result(actor.Started(data), actor.StartError),
) -> ChildSpecification(data) {
  ChildSpecification(
    start:,
    restart: Permanent,
    significant: False,
    child_type: Worker(5000),
  )
}

/// A special child that is a supervisor itself.
///
pub fn supervisor(
  run start: fn() -> Result(actor.Started(data), actor.StartError),
) -> ChildSpecification(data) {
  ChildSpecification(
    start:,
    restart: Permanent,
    significant: False,
    child_type: Supervisor,
  )
}

/// This defines if a child is considered significant for automatic
/// self-shutdown of the supervisor.
///
/// You most likely do not want to consider any children significant.
///
/// This will be ignored if the supervisor auto shutdown is set to `Never`,
/// which is the default.
///
/// The default value for significance is `False`.
pub fn significant(
  child: ChildSpecification(data),
  significant: Bool,
) -> ChildSpecification(data) {
  ChildSpecification(..child, significant: significant)
}

/// This defines the amount of milliseconds a child has to shut down before
/// being brutal killed by the supervisor.
///
/// If not set the default for a child is 5000ms.
///
/// This will be ignored if the child is a supervisor itself.
///
pub fn timeout(
  child: ChildSpecification(data),
  ms ms: Int,
) -> ChildSpecification(data) {
  case child.child_type {
    Worker(_) -> ChildSpecification(..child, child_type: Worker(ms))
    _ -> child
  }
}

/// When the child is to be restarted. See the `Restart` documentation for
/// more.
///
/// The default value for restart is `Permanent`.
pub fn restart(
  child: ChildSpecification(data),
  restart: Restart,
) -> ChildSpecification(data) {
  ChildSpecification(..child, restart: restart)
}

/// Transform the data of the started child process.
///
pub fn map_data(
  child: ChildSpecification(a),
  transform: fn(a) -> b,
) -> ChildSpecification(b) {
  ChildSpecification(..child, start: fn() {
    case child.start() {
      Ok(started) -> Ok(actor.Started(..started, data: transform(started.data)))
      Error(e) -> Error(e)
    }
  })
}
