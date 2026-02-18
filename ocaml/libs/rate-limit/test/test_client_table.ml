(* Helper to create a Key.t from a string for convenience in tests *)
let key s = Client_table.Key.{user_agent= s; host_ip= ""}

let test_create () =
  let table = Client_table.create () in
  Alcotest.(check (option int))
    "Empty table returns None for get" None
    (Client_table.get table ~client_id:{user_agent= "test"; host_ip= ""})

let test_insert () =
  let table = Client_table.create () in
  let success =
    Client_table.insert table ~client_id:{user_agent= "test"; host_ip= ""} 42
  in
  Alcotest.(check bool) "Inserting should succeed" true success ;
  Alcotest.(check (option int))
    "Get should return inserted value" (Some 42)
    (Client_table.get table ~client_id:{user_agent= "test"; host_ip= ""})

let test_insert_duplicate () =
  let table = Client_table.create () in
  let success = Client_table.insert table ~client_id:(key "agent1") 1 in
  Alcotest.(check bool) "First insert should succeed" true success ;
  let success_dup = Client_table.insert table ~client_id:(key "agent1") 2 in
  Alcotest.(check bool) "Inserting duplicate key should fail" false success_dup

let test_delete () =
  let table = Client_table.create () in
  let _ = Client_table.insert table ~client_id:(key "agent1") 42 in
  Alcotest.(check (option int))
    "Entry exists before delete" (Some 42)
    (Client_table.get table ~client_id:(key "agent1")) ;
  Client_table.delete table ~client_id:(key "agent1") ;
  Alcotest.(check (option int))
    "Entry removed after delete" None
    (Client_table.get table ~client_id:(key "agent1"))

let test_delete_nonexistent () =
  let table = Client_table.create () in
  Client_table.delete table ~client_id:(key "nonexistent") ;
  Alcotest.(check pass) "Deleting nonexistent entry should not raise" () ()

let test_get_nonexistent () =
  let table = Client_table.create () in
  Alcotest.(check (option int))
    "Get nonexistent entry returns None" None
    (Client_table.get table ~client_id:(key "nonexistent"))

let test_multiple_entries () =
  let table = Client_table.create () in
  let _ = Client_table.insert table ~client_id:(key "agent1") 10 in
  let _ = Client_table.insert table ~client_id:(key "agent2") 20 in
  Alcotest.(check (option int))
    "Agent1 has correct value" (Some 10)
    (Client_table.get table ~client_id:(key "agent1")) ;
  Alcotest.(check (option int))
    "Agent2 has correct value" (Some 20)
    (Client_table.get table ~client_id:(key "agent2"))

let test_concurrent_insert_delete_stress () =
  (* Stress test: rapidly insert and delete entries. *)
  let table = Client_table.create () in
  let iterations = 1000 in
  let num_keys = 10 in
  let errors = ref 0 in
  let errors_mutex = Mutex.create () in
  let add_threads =
    Array.init 5 (fun t ->
        Thread.create
          (fun () ->
            for i = 0 to iterations - 1 do
              let k =
                Printf.sprintf "key%d" (((t * iterations) + i) mod num_keys)
              in
              let _ = Client_table.insert table ~client_id:(key k) i in
              ()
            done
          )
          ()
    )
  in
  let delete_threads =
    Array.init 5 (fun t ->
        Thread.create
          (fun () ->
            for i = 0 to iterations - 1 do
              let k =
                Printf.sprintf "key%d" (((t * iterations) + i) mod num_keys)
              in
              Client_table.delete table ~client_id:(key k)
            done
          )
          ()
    )
  in
  let read_threads =
    Array.init 5 (fun t ->
        Thread.create
          (fun () ->
            for i = 0 to iterations - 1 do
              let k =
                Printf.sprintf "key%d" (((t * iterations) + i) mod num_keys)
              in
              (* This should never crash, even if key doesn't exist *)
              try
                let _ = Client_table.get table ~client_id:(key k) in
                ()
              with _ ->
                Mutex.lock errors_mutex ;
                incr errors ;
                Mutex.unlock errors_mutex
            done
          )
          ()
    )
  in
  Array.iter Thread.join add_threads ;
  Array.iter Thread.join delete_threads ;
  Array.iter Thread.join read_threads ;
  Alcotest.(check int) "No errors during concurrent operations" 0 !errors

let test_get_during_delete_race () =
  (* Test that get doesn't crash when entry is being deleted. *)
  let iterations = 500 in
  let errors = ref 0 in
  let errors_mutex = Mutex.create () in
  for _ = 1 to iterations do
    let table = Client_table.create () in
    let _ = Client_table.insert table ~client_id:(key "target") 42 in
    let barrier = ref 0 in
    let barrier_mutex = Mutex.create () in
    let reader =
      Thread.create
        (fun () ->
          Mutex.lock barrier_mutex ;
          incr barrier ;
          Mutex.unlock barrier_mutex ;
          while
            Mutex.lock barrier_mutex ;
            let b = !barrier in
            Mutex.unlock barrier_mutex ; b < 2
          do
            Thread.yield ()
          done ;
          try
            let _ = Client_table.get table ~client_id:(key "target") in
            ()
          with _ ->
            Mutex.lock errors_mutex ; incr errors ; Mutex.unlock errors_mutex
        )
        ()
    in
    let deleter =
      Thread.create
        (fun () ->
          Mutex.lock barrier_mutex ;
          incr barrier ;
          Mutex.unlock barrier_mutex ;
          while
            Mutex.lock barrier_mutex ;
            let b = !barrier in
            Mutex.unlock barrier_mutex ; b < 2
          do
            Thread.yield ()
          done ;
          Client_table.delete table ~client_id:(key "target")
        )
        ()
    in
    Thread.join reader ; Thread.join deleter
  done ;
  Alcotest.(check int) "No crashes during get/delete race" 0 !errors

(* Wildcard matching tests *)

let test_wildcard_user_agent_matches_any () =
  (* An entry with empty user_agent field should match any user_agent *)
  let table = Client_table.create () in
  let pattern = Client_table.Key.{user_agent= ""; host_ip= "192.168.1.1"} in
  let _ = Client_table.insert table ~client_id:pattern 1 in
  (* Should match any user_agent with same host_ip *)
  let client1 = Client_table.Key.{user_agent= "curl"; host_ip= "192.168.1.1"} in
  let client2 = Client_table.Key.{user_agent= "wget"; host_ip= "192.168.1.1"} in
  let client3 = Client_table.Key.{user_agent= ""; host_ip= "192.168.1.1"} in
  Alcotest.(check bool)
    "wildcard user_agent matches curl" true
    (Client_table.mem table ~client_id:client1) ;
  Alcotest.(check bool)
    "wildcard user_agent matches wget" true
    (Client_table.mem table ~client_id:client2) ;
  Alcotest.(check bool)
    "wildcard user_agent matches empty" true
    (Client_table.mem table ~client_id:client3) ;
  (* Should not match different host_ip *)
  let client_other =
    Client_table.Key.{user_agent= "curl"; host_ip= "10.0.0.1"}
  in
  Alcotest.(check bool)
    "{user_agent=curl, host_ip=10.0.0.1} does not match {user_agent=*, \
     host_ip=192.168.1.1}"
    false
    (Client_table.mem table ~client_id:client_other)

let test_wildcard_host_ip_matches_any () =
  (* An entry with empty host_ip should match any host_ip *)
  let table = Client_table.create () in
  let pattern = Client_table.Key.{user_agent= "curl"; host_ip= ""} in
  let _ = Client_table.insert table ~client_id:pattern 1 in
  (* Should match any host_ip with same user_agent *)
  let client1 = Client_table.Key.{user_agent= "curl"; host_ip= "192.168.1.1"} in
  let client2 = Client_table.Key.{user_agent= "curl"; host_ip= "10.0.0.1"} in
  let client3 = Client_table.Key.{user_agent= "curl"; host_ip= ""} in
  Alcotest.(check bool)
    "wildcard host_ip matches 192.168.1.1" true
    (Client_table.mem table ~client_id:client1) ;
  Alcotest.(check bool)
    "wildcard host_ip matches 10.0.0.1" true
    (Client_table.mem table ~client_id:client2) ;
  Alcotest.(check bool)
    "wildcard host_ip matches empty" true
    (Client_table.mem table ~client_id:client3) ;
  (* Should not match different user_agent *)
  let client_other =
    Client_table.Key.{user_agent= "wget"; host_ip= "192.168.1.1"}
  in
  Alcotest.(check bool)
    "wildcard does not match different user_agent" false
    (Client_table.mem table ~client_id:client_other)

let test_wildcard_match_priority_exact_first () =
  (* Exact match should take priority over wildcards *)
  let table = Client_table.create () in
  let exact = Client_table.Key.{user_agent= "curl"; host_ip= "192.168.1.1"} in
  let wildcard_ua = Client_table.Key.{user_agent= ""; host_ip= "192.168.1.1"} in
  let wildcard_ip = Client_table.Key.{user_agent= "curl"; host_ip= ""} in
  (* Add in reverse priority order to test sorting *)
  let _ = Client_table.insert table ~client_id:wildcard_ua 5 in
  let _ = Client_table.insert table ~client_id:wildcard_ip 15 in
  let _ = Client_table.insert table ~client_id:exact 10 in
  (* Lookup with exact key should return exact entry (10), not wildcards *)
  let client = Client_table.Key.{user_agent= "curl"; host_ip= "192.168.1.1"} in
  Alcotest.(check (option int))
    "exact match takes priority" (Some 10)
    (Client_table.get table ~client_id:client)

let test_wildcard_match_priority_host_ip_over_user_agent () =
  (* host_ip wildcard (user_agent specified) should match before
     user_agent wildcard (host_ip specified) *)
  let table = Client_table.create () in
  let wildcard_ua = Client_table.Key.{user_agent= ""; host_ip= "192.168.1.1"} in
  let wildcard_ip = Client_table.Key.{user_agent= "curl"; host_ip= ""} in
  (* Add user_agent wildcard first *)
  let _ = Client_table.insert table ~client_id:wildcard_ua 5 in
  (* Add host_ip wildcard second *)
  let _ = Client_table.insert table ~client_id:wildcard_ip 15 in
  (* Lookup should prefer host_ip wildcard (15) over user_agent wildcard (5) *)
  let client = Client_table.Key.{user_agent= "curl"; host_ip= "192.168.1.1"} in
  Alcotest.(check (option int))
    "host_ip wildcard takes priority over user_agent wildcard" (Some 15)
    (Client_table.get table ~client_id:client)

let test_no_spurious_wildcard_matches () =
  (* Ensure wildcards don't match when they shouldn't *)
  let table = Client_table.create () in
  let pattern1 =
    Client_table.Key.{user_agent= "curl"; host_ip= "192.168.1.1"}
  in
  let pattern2 = Client_table.Key.{user_agent= "wget"; host_ip= ""} in
  let _ = Client_table.insert table ~client_id:pattern1 10 in
  let _ = Client_table.insert table ~client_id:pattern2 20 in
  (* Client with different user_agent and host_ip should not match pattern1 *)
  let client1 = Client_table.Key.{user_agent= "curl"; host_ip= "10.0.0.1"} in
  Alcotest.(check bool)
    "{user_agent=curl, host_ip=10.0.0.1} does not match {user_agent=curl, \
     host_ip=192.168.1.1}"
    false
    (Client_table.mem table ~client_id:client1) ;
  (* Client with matching user_agent but different host_ip should match pattern2 *)
  let client2 = Client_table.Key.{user_agent= "wget"; host_ip= "10.0.0.1"} in
  Alcotest.(check (option int))
    "{user_agent=wget, host_ip=10.0.0.1} matches {user_agent=wget, host_ip=*} \
     wildcard"
    (Some 20)
    (Client_table.get table ~client_id:client2) ;
  (* Client with no matching pattern *)
  let client3 =
    Client_table.Key.{user_agent= "firefox"; host_ip= "172.16.0.1"}
  in
  Alcotest.(check bool)
    "{user_agent=firefox, host_ip=172.16.0.1} has no match" false
    (Client_table.mem table ~client_id:client3)

let test_lru_cache_overflow () =
  (* The internal LRU cache has capacity 100. Insert a wildcard entry and
     perform >100 distinct lookups so the cache overflows, then verify
     lookups still return correct results after evictions. *)
  let table = Client_table.create () in
  let pattern = Client_table.Key.{user_agent= "curl"; host_ip= ""} in
  let _ = Client_table.insert table ~client_id:pattern 42 in
  (* Perform 150 distinct lookups to overflow the cache *)
  for i = 1 to 150 do
    let client =
      Client_table.Key.
        {user_agent= "curl"; host_ip= Printf.sprintf "10.0.0.%d" i}
    in
    Alcotest.(check (option int))
      (Printf.sprintf "lookup %d returns correct value" i)
      (Some 42)
      (Client_table.get table ~client_id:client)
  done ;
  (* Re-check the first few lookups which should have been evicted from
     the cache; they must still resolve correctly via the table scan. *)
  for i = 1 to 10 do
    let client =
      Client_table.Key.
        {user_agent= "curl"; host_ip= Printf.sprintf "10.0.0.%d" i}
    in
    Alcotest.(check (option int))
      (Printf.sprintf "re-lookup %d after eviction" i)
      (Some 42)
      (Client_table.get table ~client_id:client)
  done ;
  (* Also check that a non-matching lookup still returns None *)
  let miss = Client_table.Key.{user_agent= "wget"; host_ip= "10.0.0.1"} in
  Alcotest.(check (option int))
    "non-matching lookup after overflow" None
    (Client_table.get table ~client_id:miss)

let test_reject_all_wildcard_key () =
  (* Keys with both fields empty should be rejected *)
  let table = Client_table.create () in
  let all_wildcard = Client_table.Key.{user_agent= ""; host_ip= ""} in
  let success = Client_table.insert table ~client_id:all_wildcard 1 in
  Alcotest.(check bool) "all-wildcard key rejected" false success

let test =
  [
    ("Create empty table", `Quick, test_create)
  ; ("Insert entry", `Quick, test_insert)
  ; ("Insert duplicate", `Quick, test_insert_duplicate)
  ; ("Delete entry", `Quick, test_delete)
  ; ("Delete nonexistent entry", `Quick, test_delete_nonexistent)
  ; ("Get nonexistent", `Quick, test_get_nonexistent)
  ; ("Multiple entries", `Quick, test_multiple_entries)
  ; ( "Concurrent insert/delete stress"
    , `Quick
    , test_concurrent_insert_delete_stress
    )
  ; ("Get during delete race", `Quick, test_get_during_delete_race)
  ; ( "Wildcard user_agent matches any"
    , `Quick
    , test_wildcard_user_agent_matches_any
    )
  ; ("Wildcard host_ip matches any", `Quick, test_wildcard_host_ip_matches_any)
  ; ( "Wildcard priority: exact first"
    , `Quick
    , test_wildcard_match_priority_exact_first
    )
  ; ( "Wildcard priority: host_ip over user_agent"
    , `Quick
    , test_wildcard_match_priority_host_ip_over_user_agent
    )
  ; ("No spurious wildcard matches", `Quick, test_no_spurious_wildcard_matches)
  ; ("Reject all-wildcard key", `Quick, test_reject_all_wildcard_key)
  ; ("LRU cache overflow", `Quick, test_lru_cache_overflow)
  ]

let () = Alcotest.run "Client table library" [("Client table tests", test)]
