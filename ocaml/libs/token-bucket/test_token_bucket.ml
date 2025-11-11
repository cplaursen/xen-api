open Token_bucket
open Thread

(* Properties to test:
   - Consuming always removes the correct amount from bucket
   - Consuming more than available tokens returns false and leaves tokens
   unchanged
   - Consuming refills bucket since last timestamp before removing
   - Peek returns min burst_size (tokens + time delta)
   - Concurrent accesses don't override each other
   - Sleeping for time t results in t * fill_rate tokens being added
*)

let test_sleep () =
  let tb = Token_bucket.create ~burst_size:20.0 ~fill_rate:5.0 in
  let _ = Token_bucket.consume tb 10.0 in
  Thread.delay 1.0 ;
  Alcotest.(check (float 0.1))
    "Sleep 1 should refill token bucket by fill_rate" 15.0 (Token_bucket.peek tb)

let test_concurrent_access () =
  let tb =
    Token_bucket.create_with_timestamp Mtime.Span.zero ~burst_size:15.0
      ~fill_rate:0.0
  in
  let threads =
    Array.init 10 (fun _ ->
        create
          (fun () ->
            Token_bucket.consume_with_timestamp
              (fun () -> Mtime.Span.zero)
              tb 1.0
          )
          ()
    )
  in
  Array.iter join threads ;
  Alcotest.(check (float 0.0))
    "Threads consuming concurrently should all remove from token amount"
    (Token_bucket.peek_with_timestamp Mtime.Span.zero tb)
    5.0

let test =
  [
    ("Refill after sleep", `Slow, test_sleep)
  ; ("Concurrent access", `Quick, test_concurrent_access)
  ]

let () = Alcotest.run "Token bucket library" [("token bucket", test)]
