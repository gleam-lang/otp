// Testing
// pub fn start_child1(x: Nil) -> Result(Channel(msg), ExitReason) {
//   todo
// }
// pub fn start_child2(_older: Pid) -> Result(Channel(msg), ExitReason) {
//   todo
// }
// pub fn start_child3(_older: Pid) -> Result(Channel(msg), ExitReason) {
//   todo
// }
// pub fn init(spec) {
//   spec
//   |> add(
//     worker_child(start_child1)
//     |> update_argument(fn(_arg, pid) { pid }),
//   )
//   |> add(worker_child(start_child2))
//   |> add(worker_child(start_child3))
// }
