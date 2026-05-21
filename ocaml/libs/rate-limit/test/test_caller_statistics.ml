module Caller_statistics = Xapi_rate_limit.Caller_statistics

let test_initial_state () =
  let cs = Caller_statistics.create ~caller_uuid:"uuid-1" in
  Alcotest.(check string)
    "uuid is stored" "uuid-1"
    (Caller_statistics.get_uuid cs) ;
  Alcotest.(check int)
    "initial call count is 0" 0
    (Caller_statistics.get_call_count cs) ;
  Alcotest.(check (float 0.0))
    "initial token count is 0.0" 0.0
    (Caller_statistics.get_token_count cs) ;
  Alcotest.(check bool)
    "initial last_called is zero" true
    (Mtime.Span.equal Mtime.Span.zero (Caller_statistics.get_last_called cs))

let test_single_register () =
  let cs = Caller_statistics.create ~caller_uuid:"uuid-2" in
  let ts = Mtime.Span.of_uint64_ns 42L in
  Caller_statistics.register_call_timestamp ~token_amount:2.5 ~timestamp:ts cs ;
  Alcotest.(check int)
    "count incremented" 1
    (Caller_statistics.get_call_count cs) ;
  Alcotest.(check (float 0.0))
    "tokens accumulated" 2.5
    (Caller_statistics.get_token_count cs) ;
  Alcotest.(check bool)
    "last_called updated" true
    (Mtime.Span.equal ts (Caller_statistics.get_last_called cs))

let test_last_called_is_max () =
  let cs = Caller_statistics.create ~caller_uuid:"uuid-3" in
  let early = Mtime.Span.of_uint64_ns 10L in
  let late = Mtime.Span.of_uint64_ns 1_000_000L in
  Caller_statistics.register_call_timestamp ~token_amount:1.0 ~timestamp:late cs ;
  Caller_statistics.register_call_timestamp ~token_amount:1.0 ~timestamp:early
    cs ;
  Alcotest.(check bool)
    "older timestamp does not overwrite newer one" true
    (Mtime.Span.equal late (Caller_statistics.get_last_called cs)) ;
  Alcotest.(check int)
    "both calls counted" 2
    (Caller_statistics.get_call_count cs)

(* Many threads, each registering many calls. The CAS retry loop must not lose
   any updates — the final counts must equal the sum of every thread's
   contributions. *)
let test_concurrent_register () =
  let cs = Caller_statistics.create ~caller_uuid:"uuid-4" in
  let num_threads = 16 in
  let calls_per_thread = 500 in
  let token_per_call = 0.25 in
  let ts = Mtime.Span.of_uint64_ns 1L in
  let threads =
    Array.init num_threads (fun _ ->
        Thread.create
          (fun () ->
            for _ = 1 to calls_per_thread do
              Caller_statistics.register_call_timestamp
                ~token_amount:token_per_call ~timestamp:ts cs
            done
          )
          ()
    )
  in
  Array.iter Thread.join threads ;
  let total_calls = num_threads * calls_per_thread in
  Alcotest.(check int)
    "no register_call updates were lost under contention" total_calls
    (Caller_statistics.get_call_count cs) ;
  Alcotest.(check (float 1e-6))
    "token sum matches total contributions"
    (float_of_int total_calls *. token_per_call)
    (Caller_statistics.get_token_count cs)

(* Threads register with monotonically increasing timestamps in arbitrary
   interleavings. Whatever the schedule, the final [last_called] must equal
   the maximum timestamp any thread used. *)
let test_concurrent_last_called_is_max () =
  let cs = Caller_statistics.create ~caller_uuid:"uuid-5" in
  let num_threads = 8 in
  let calls_per_thread = 200 in
  let max_ns = Int64.of_int ((num_threads * calls_per_thread) + 1_000) in
  let threads =
    Array.init num_threads (fun thread_id ->
        Thread.create
          (fun () ->
            for i = 1 to calls_per_thread do
              let ns = Int64.of_int ((thread_id * calls_per_thread) + i) in
              let ts = Mtime.Span.of_uint64_ns ns in
              Caller_statistics.register_call_timestamp ~token_amount:1.0
                ~timestamp:ts cs
            done
          )
          ()
    )
  in
  (* Also write the global max from the main thread to ensure max wins
     regardless of which thread's update lands last. *)
  let main_max = Mtime.Span.of_uint64_ns max_ns in
  Caller_statistics.register_call_timestamp ~token_amount:1.0
    ~timestamp:main_max cs ;
  Array.iter Thread.join threads ;
  Alcotest.(check bool)
    "last_called equals the overall maximum timestamp" true
    (Mtime.Span.equal main_max (Caller_statistics.get_last_called cs)) ;
  Alcotest.(check int)
    "all calls accounted for"
    ((num_threads * calls_per_thread) + 1)
    (Caller_statistics.get_call_count cs)

(* Readers running concurrently with writers should never observe a torn
   value: call_count and token_count come from the same atomic snapshot,
   so token_count >= call_count * token_per_call at every observation. *)
let test_concurrent_reads_see_consistent_snapshot () =
  let cs = Caller_statistics.create ~caller_uuid:"uuid-6" in
  let num_writers = 4 in
  let calls_per_writer = 1_000 in
  let token_per_call = 1.0 in
  let stop = Atomic.make false in
  let inconsistencies = Atomic.make 0 in
  let ts = Mtime.Span.of_uint64_ns 1L in
  let reader () =
    while not (Atomic.get stop) do
      let count = Caller_statistics.get_call_count cs in
      let tokens = Caller_statistics.get_token_count cs in
      let expected = float_of_int count *. token_per_call in
      if abs_float (tokens -. expected) > 1e-9 then Atomic.incr inconsistencies
    done
  in
  let writers =
    Array.init num_writers (fun _ ->
        Thread.create
          (fun () ->
            for _ = 1 to calls_per_writer do
              Caller_statistics.register_call_timestamp
                ~token_amount:token_per_call ~timestamp:ts cs
            done
          )
          ()
    )
  in
  let readers = Array.init 4 (fun _ -> Thread.create reader ()) in
  Array.iter Thread.join writers ;
  Atomic.set stop true ;
  Array.iter Thread.join readers ;
  Alcotest.(check int)
    "readers never observed a torn count/token snapshot" 0
    (Atomic.get inconsistencies) ;
  Alcotest.(check int)
    "all writer updates landed"
    (num_writers * calls_per_writer)
    (Caller_statistics.get_call_count cs)

let test =
  [
    ("Initial state", `Quick, test_initial_state)
  ; ("Single register updates all fields", `Quick, test_single_register)
  ; ("last_called keeps the maximum timestamp", `Quick, test_last_called_is_max)
  ; ( "Concurrent register_call loses no updates"
    , `Quick
    , test_concurrent_register
    )
  ; ( "Concurrent last_called converges to global max"
    , `Quick
    , test_concurrent_last_called_is_max
    )
  ; ( "Concurrent readers see a consistent snapshot"
    , `Quick
    , test_concurrent_reads_see_consistent_snapshot
    )
  ]

let () =
  Alcotest.run "Caller statistics library" [("Caller statistics tests", test)]
