open Bechamel

let run () =
  let _ : bool = Sys.opaque_identity (Pool_role.is_master ()) in
  ()

let mutex_workload =
  Bechamel_simple_cli.thread_workload ~before:ignore ~after:ignore ~run

let benchmarks =
  [Test.make ~name:"Pool_role.is_master" (Staged.stage Pool_role.is_master)]

let () = Bechamel_simple_cli.cli ~workloads:[mutex_workload] benchmarks
