let key ua ip = Client_table.Key.{user_agent= ua; host_ip= ip}

let check_stats msg ~call_count ~tokens_consumed stats =
  Alcotest.(check int)
    (msg ^ ": call_count") call_count stats.Client_tracker.call_count ;
  Alcotest.(check (float 0.001))
    (msg ^ ": tokens_consumed")
    tokens_consumed stats.Client_tracker.tokens_consumed

(* --- Basic operations --- *)

let test_create () =
  let t = Client_tracker.create () in
  Alcotest.(check bool)
    "empty tracker has no members" false
    (Client_tracker.mem t ~client_id:(key "curl" "1.2.3.4"))

let test_add_and_mem () =
  let t = Client_tracker.create () in
  let ok = Client_tracker.add_client t ~client_id:(key "curl" "") () in
  Alcotest.(check bool) "add_client succeeds" true ok ;
  Alcotest.(check bool)
    "mem finds added client" true
    (Client_tracker.mem t ~client_id:(key "curl" "1.2.3.4"))

let test_add_duplicate () =
  let t = Client_tracker.create () in
  let _ = Client_tracker.add_client t ~client_id:(key "curl" "") () in
  let ok = Client_tracker.add_client t ~client_id:(key "curl" "") () in
  Alcotest.(check bool) "duplicate add fails" false ok

let test_add_all_wildcard_rejected () =
  let t = Client_tracker.create () in
  let ok = Client_tracker.add_client t ~client_id:(key "" "") () in
  Alcotest.(check bool) "all-wildcard key rejected" false ok

let test_remove_client () =
  let t = Client_tracker.create () in
  let _ = Client_tracker.add_client t ~client_id:(key "curl" "") () in
  Client_tracker.remove_client t ~client_id:(key "curl" "") ;
  Alcotest.(check bool)
    "client removed" false
    (Client_tracker.mem t ~client_id:(key "curl" "1.2.3.4"))

let test_remove_nonexistent () =
  let t = Client_tracker.create () in
  Client_tracker.remove_client t ~client_id:(key "curl" "") ;
  Alcotest.(check pass) "removing nonexistent client does not raise" () ()

let test_get_stats_empty () =
  let t = Client_tracker.create () in
  Alcotest.(check bool)
    "no stats for unknown client" true
    (Client_tracker.get_stats t ~client_id:(key "curl" "1.2.3.4") = None)

let test_get_stats_no_calls () =
  let t = Client_tracker.create () in
  let _ = Client_tracker.add_client t ~client_id:(key "curl" "") () in
  let stats =
    Option.get (Client_tracker.get_stats t ~client_id:(key "curl" "1.2.3.4"))
  in
  check_stats "fresh client" ~call_count:0 ~tokens_consumed:0.0 stats ;
  Alcotest.(check bool) "last_called is None" true (stats.last_called = None)

(* --- Token tracking --- *)

let test_submit_tracks_tokens () =
  let t = Client_tracker.create () in
  let _ = Client_tracker.add_client t ~client_id:(key "curl" "") () in
  let id = key "curl" "1.2.3.4" in
  Client_tracker.submit_async t ~client_id:id ~callback:(fun () -> ()) 3.0 ;
  Client_tracker.submit_async t ~client_id:id ~callback:(fun () -> ()) 7.0 ;
  Client_tracker.submit_async t ~client_id:id ~callback:(fun () -> ()) 2.5 ;
  let stats = Option.get (Client_tracker.get_stats t ~client_id:id) in
  check_stats "after 3 calls" ~call_count:3 ~tokens_consumed:12.5 stats ;
  Alcotest.(check bool) "last_called is set" true (stats.last_called <> None)

let test_submit_sync_tracks_tokens () =
  let t = Client_tracker.create () in
  let _ = Client_tracker.add_client t ~client_id:(key "curl" "") () in
  let id = key "curl" "1.2.3.4" in
  let r1 =
    Client_tracker.submit_sync t ~client_id:id ~callback:(fun () -> 42) 5.0
  in
  Alcotest.(check int) "sync returns result" 42 r1 ;
  let r2 =
    Client_tracker.submit_sync t ~client_id:id ~callback:(fun () -> "hello") 3.0
  in
  Alcotest.(check string) "sync returns string" "hello" r2 ;
  let stats = Option.get (Client_tracker.get_stats t ~client_id:id) in
  check_stats "after 2 sync calls" ~call_count:2 ~tokens_consumed:8.0 stats

(* --- Auto-add on submit --- *)

let test_auto_add_on_submit_async () =
  let t = Client_tracker.create () in
  let id = key "curl" "1.2.3.4" in
  Alcotest.(check bool)
    "not present before" false
    (Client_tracker.mem t ~client_id:id) ;
  Client_tracker.submit_async t ~client_id:id ~callback:(fun () -> ()) 1.0 ;
  Alcotest.(check bool)
    "auto-added after submit" true
    (Client_tracker.mem t ~client_id:id) ;
  let stats = Option.get (Client_tracker.get_stats t ~client_id:id) in
  check_stats "auto-added has 1 call" ~call_count:1 ~tokens_consumed:1.0 stats

let test_auto_add_on_submit_sync () =
  let t = Client_tracker.create () in
  let id = key "wget" "10.0.0.1" in
  let result =
    Client_tracker.submit_sync t ~client_id:id ~callback:(fun () -> 99) 2.0
  in
  Alcotest.(check int) "sync callback runs" 99 result ;
  let stats = Option.get (Client_tracker.get_stats t ~client_id:id) in
  check_stats "auto-added sync" ~call_count:1 ~tokens_consumed:2.0 stats

let test_auto_add_no_rate_limiter () =
  let t = Client_tracker.create () in
  let id = key "curl" "1.2.3.4" in
  let executed = ref false in
  Client_tracker.submit_async t ~client_id:id
    ~callback:(fun () -> executed := true)
    1.0 ;
  Alcotest.(check bool)
    "callback runs immediately (no rate limiter)" true !executed

let test_auto_add_matches_existing_wildcard () =
  let t = Client_tracker.create () in
  let _ = Client_tracker.add_client t ~client_id:(key "curl" "") () in
  (* Submit with a specific key that matches the wildcard rule *)
  Client_tracker.submit_async t ~client_id:(key "curl" "1.2.3.4")
    ~callback:(fun () -> ())
    5.0 ;
  (* The call should be recorded on the wildcard entry, not auto-added *)
  let stats =
    Option.get (Client_tracker.get_stats t ~client_id:(key "curl" "1.2.3.4"))
  in
  check_stats "wildcard entry got the call" ~call_count:1 ~tokens_consumed:5.0
    stats ;
  (* Remove the wildcard rule; if a separate exact entry had been auto-added
     for "curl"/"1.2.3.4", it would still be found after removing the wildcard *)
  Client_tracker.remove_client t ~client_id:(key "curl" "") ;
  Alcotest.(check bool)
    "no separate entry was auto-added" true
    (Client_tracker.get_stats t ~client_id:(key "curl" "1.2.3.4") = None)

(* --- Sliding window --- *)

let test_sliding_window_prunes_old_calls () =
  (* Use a very short 200ms window *)
  let window = Mtime.Span.of_uint64_ns 200_000_000L in
  let t = Client_tracker.create ~window () in
  let _ = Client_tracker.add_client t ~client_id:(key "curl" "") () in
  let id = key "curl" "1.2.3.4" in
  Client_tracker.submit_async t ~client_id:id ~callback:(fun () -> ()) 3.0 ;
  Client_tracker.submit_async t ~client_id:id ~callback:(fun () -> ()) 4.0 ;
  let stats_before = Option.get (Client_tracker.get_stats t ~client_id:id) in
  check_stats "before expiry" ~call_count:2 ~tokens_consumed:7.0 stats_before ;
  (* Wait for the window to expire *)
  Thread.delay 0.3 ;
  let stats_after = Option.get (Client_tracker.get_stats t ~client_id:id) in
  check_stats "after expiry" ~call_count:0 ~tokens_consumed:0.0 stats_after ;
  (* last_called should still be set even after window pruning *)
  Alcotest.(check bool)
    "last_called persists after pruning" true
    (stats_after.last_called <> None)

let test_sliding_window_partial_expiry () =
  (* Use a 300ms window *)
  let window = Mtime.Span.of_uint64_ns 300_000_000L in
  let t = Client_tracker.create ~window () in
  let _ = Client_tracker.add_client t ~client_id:(key "curl" "") () in
  let id = key "curl" "1.2.3.4" in
  (* First batch of calls *)
  Client_tracker.submit_async t ~client_id:id ~callback:(fun () -> ()) 2.0 ;
  Client_tracker.submit_async t ~client_id:id ~callback:(fun () -> ()) 3.0 ;
  (* Wait 200ms - still within window *)
  Thread.delay 0.2 ;
  (* Second batch *)
  Client_tracker.submit_async t ~client_id:id ~callback:(fun () -> ()) 5.0 ;
  (* Wait another 200ms - first batch should expire, second still valid *)
  Thread.delay 0.2 ;
  let stats = Option.get (Client_tracker.get_stats t ~client_id:id) in
  Alcotest.(check int) "only recent calls remain" 1 stats.call_count ;
  Alcotest.(check (float 0.001))
    "only recent tokens remain" 5.0 stats.tokens_consumed

let test_default_window_is_one_hour () =
  let t = Client_tracker.create () in
  let _ = Client_tracker.add_client t ~client_id:(key "curl" "") () in
  let id = key "curl" "1.2.3.4" in
  Client_tracker.submit_async t ~client_id:id ~callback:(fun () -> ()) 1.0 ;
  (* With a 1 hour window, the call should still be present *)
  let stats = Option.get (Client_tracker.get_stats t ~client_id:id) in
  check_stats "call within default 1h window" ~call_count:1 ~tokens_consumed:1.0
    stats

(* --- Rate limiter integration --- *)

let test_set_rate_limiter () =
  let t = Client_tracker.create () in
  let _ = Client_tracker.add_client t ~client_id:(key "curl" "") () in
  let rl = Rate_limit.create ~burst_size:10.0 ~fill_rate:10.0 in
  let ok = Client_tracker.set_rate_limiter t ~client_id:(key "curl" "") rl in
  Alcotest.(check bool) "set_rate_limiter succeeds" true ok ;
  Client_tracker.remove_client t ~client_id:(key "curl" "")

let test_set_rate_limiter_nonexistent () =
  let t = Client_tracker.create () in
  let rl = Rate_limit.create ~burst_size:10.0 ~fill_rate:10.0 in
  let ok = Client_tracker.set_rate_limiter t ~client_id:(key "curl" "") rl in
  Alcotest.(check bool) "set_rate_limiter fails for nonexistent" false ok ;
  Rate_limit.delete rl

let test_remove_rate_limiter () =
  let t = Client_tracker.create () in
  let rl = Rate_limit.create ~burst_size:10.0 ~fill_rate:10.0 in
  let _ =
    Client_tracker.add_client t ~client_id:(key "curl" "") ~rate_limiter:rl ()
  in
  Client_tracker.remove_rate_limiter t ~client_id:(key "curl" "") ;
  (* After removing limiter, submits should run immediately *)
  let executed = ref false in
  Client_tracker.submit_async t ~client_id:(key "curl" "1.2.3.4")
    ~callback:(fun () -> executed := true)
    1.0 ;
  Alcotest.(check bool)
    "callback runs immediately after limiter removed" true !executed ;
  Client_tracker.remove_client t ~client_id:(key "curl" "")

let test_submit_with_rate_limiter () =
  let t = Client_tracker.create () in
  let rl = Rate_limit.create ~burst_size:10.0 ~fill_rate:10.0 in
  let _ =
    Client_tracker.add_client t ~client_id:(key "curl" "") ~rate_limiter:rl ()
  in
  let id = key "curl" "1.2.3.4" in
  (* Drain the bucket *)
  Client_tracker.submit_async t ~client_id:id ~callback:(fun () -> ()) 10.0 ;
  (* Next submit should be rate limited (non-blocking but queued) *)
  let executed = ref false in
  let start = Mtime_clock.counter () in
  Client_tracker.submit_async t ~client_id:id
    ~callback:(fun () -> executed := true)
    5.0 ;
  let elapsed = Mtime.Span.to_float_ns (Mtime_clock.count start) *. 1e-9 in
  Alcotest.(check bool) "submit_async returns immediately" true (elapsed < 0.1) ;
  (* Wait for worker to process *)
  Thread.delay 0.6 ;
  Alcotest.(check bool) "callback eventually executed" true !executed ;
  (* Stats should reflect both calls *)
  let stats = Option.get (Client_tracker.get_stats t ~client_id:id) in
  check_stats "rate limited calls tracked" ~call_count:2 ~tokens_consumed:15.0
    stats ;
  Client_tracker.remove_client t ~client_id:(key "curl" "")

let test_submit_sync_with_rate_limiter () =
  let t = Client_tracker.create () in
  let rl = Rate_limit.create ~burst_size:10.0 ~fill_rate:10.0 in
  let _ =
    Client_tracker.add_client t ~client_id:(key "curl" "") ~rate_limiter:rl ()
  in
  let id = key "curl" "1.2.3.4" in
  let result =
    Client_tracker.submit_sync t ~client_id:id ~callback:(fun () -> 42) 5.0
  in
  Alcotest.(check int) "sync returns result" 42 result ;
  (* Drain the bucket *)
  Client_tracker.submit_async t ~client_id:id ~callback:(fun () -> ()) 5.0 ;
  let start = Mtime_clock.counter () in
  let result2 =
    Client_tracker.submit_sync t ~client_id:id ~callback:(fun () -> "done") 5.0
  in
  let elapsed = Mtime.Span.to_float_ns (Mtime_clock.count start) *. 1e-9 in
  Alcotest.(check string) "sync returns after wait" "done" result2 ;
  Alcotest.(check bool) "sync blocked for tokens" true (elapsed >= 0.4) ;
  Client_tracker.remove_client t ~client_id:(key "curl" "")

(* --- Multiple clients --- *)

let test_multiple_clients_independent_stats () =
  let t = Client_tracker.create () in
  let _ = Client_tracker.add_client t ~client_id:(key "curl" "") () in
  let _ = Client_tracker.add_client t ~client_id:(key "wget" "") () in
  let curl_id = key "curl" "1.2.3.4" in
  let wget_id = key "wget" "1.2.3.4" in
  Client_tracker.submit_async t ~client_id:curl_id ~callback:(fun () -> ()) 3.0 ;
  Client_tracker.submit_async t ~client_id:curl_id ~callback:(fun () -> ()) 2.0 ;
  Client_tracker.submit_async t ~client_id:wget_id ~callback:(fun () -> ()) 7.0 ;
  let curl_stats = Option.get (Client_tracker.get_stats t ~client_id:curl_id) in
  let wget_stats = Option.get (Client_tracker.get_stats t ~client_id:wget_id) in
  check_stats "curl" ~call_count:2 ~tokens_consumed:5.0 curl_stats ;
  check_stats "wget" ~call_count:1 ~tokens_consumed:7.0 wget_stats

(* --- Concurrent operations --- *)

let test_concurrent_submits () =
  let t = Client_tracker.create () in
  let _ = Client_tracker.add_client t ~client_id:(key "curl" "") () in
  let id = key "curl" "1.2.3.4" in
  let num_threads = 10 in
  let calls_per_thread = 100 in
  let threads =
    Array.init num_threads (fun _ ->
        Thread.create
          (fun () ->
            for _ = 1 to calls_per_thread do
              Client_tracker.submit_async t ~client_id:id
                ~callback:(fun () -> ())
                1.0
            done
          )
          ()
    )
  in
  Array.iter Thread.join threads ;
  let stats = Option.get (Client_tracker.get_stats t ~client_id:id) in
  let expected = num_threads * calls_per_thread in
  Alcotest.(check int) "all concurrent calls recorded" expected stats.call_count ;
  Alcotest.(check (float 0.001))
    "all tokens recorded" (float_of_int expected) stats.tokens_consumed

let test_concurrent_auto_add () =
  let t = Client_tracker.create () in
  let num_threads = 20 in
  let threads =
    Array.init num_threads (fun i ->
        Thread.create
          (fun () ->
            let id = key "curl" (Printf.sprintf "10.0.0.%d" i) in
            Client_tracker.submit_async t ~client_id:id
              ~callback:(fun () -> ())
              1.0
          )
          ()
    )
  in
  Array.iter Thread.join threads ;
  (* Each distinct key should have been auto-added *)
  for i = 0 to num_threads - 1 do
    let id = key "curl" (Printf.sprintf "10.0.0.%d" i) in
    Alcotest.(check bool)
      (Printf.sprintf "client %d exists" i)
      true
      (Client_tracker.mem t ~client_id:id)
  done

(* --- Remove cleans up rate limiter --- *)

let test_remove_cleans_up_rate_limiter () =
  let t = Client_tracker.create () in
  let rl = Rate_limit.create ~burst_size:10.0 ~fill_rate:10.0 in
  let _ =
    Client_tracker.add_client t ~client_id:(key "curl" "") ~rate_limiter:rl ()
  in
  (* remove_client should delete the rate limiter without crashing *)
  Client_tracker.remove_client t ~client_id:(key "curl" "") ;
  Alcotest.(check bool)
    "client removed after cleanup" false
    (Client_tracker.mem t ~client_id:(key "curl" "1.2.3.4"))

let test =
  [
    ("Create empty tracker", `Quick, test_create)
  ; ("Add client and mem", `Quick, test_add_and_mem)
  ; ("Add duplicate", `Quick, test_add_duplicate)
  ; ("Reject all-wildcard key", `Quick, test_add_all_wildcard_rejected)
  ; ("Remove client", `Quick, test_remove_client)
  ; ("Remove nonexistent", `Quick, test_remove_nonexistent)
  ; ("Get stats for unknown", `Quick, test_get_stats_empty)
  ; ("Get stats no calls", `Quick, test_get_stats_no_calls)
  ; ("Submit tracks tokens", `Quick, test_submit_tracks_tokens)
  ; ("Submit sync tracks tokens", `Quick, test_submit_sync_tracks_tokens)
  ; ("Auto-add on submit_async", `Quick, test_auto_add_on_submit_async)
  ; ("Auto-add on submit_sync", `Quick, test_auto_add_on_submit_sync)
  ; ("Auto-add has no rate limiter", `Quick, test_auto_add_no_rate_limiter)
  ; ( "Auto-add matches existing wildcard"
    , `Quick
    , test_auto_add_matches_existing_wildcard
    )
  ; ( "Sliding window prunes old calls"
    , `Slow
    , test_sliding_window_prunes_old_calls
    )
  ; ("Sliding window partial expiry", `Slow, test_sliding_window_partial_expiry)
  ; ("Default window is 1 hour", `Quick, test_default_window_is_one_hour)
  ; ("Set rate limiter", `Quick, test_set_rate_limiter)
  ; ("Set rate limiter nonexistent", `Quick, test_set_rate_limiter_nonexistent)
  ; ("Remove rate limiter", `Quick, test_remove_rate_limiter)
  ; ("Submit with rate limiter", `Slow, test_submit_with_rate_limiter)
  ; ("Submit sync with rate limiter", `Slow, test_submit_sync_with_rate_limiter)
  ; ( "Multiple clients independent stats"
    , `Quick
    , test_multiple_clients_independent_stats
    )
  ; ("Concurrent submits", `Quick, test_concurrent_submits)
  ; ("Concurrent auto-add", `Quick, test_concurrent_auto_add)
  ; ("Remove cleans up rate limiter", `Quick, test_remove_cleans_up_rate_limiter)
  ]

let () = Alcotest.run "Client tracker library" [("Client tracker tests", test)]
