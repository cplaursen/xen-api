(*
 * Copyright (C) 2025 Cloud Software Group
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

let with_lock = Xapi_stdext_threads.Threadext.Mutex.execute

type call_record = {timestamp: Mtime.span; amount: float}

type stats = {
    call_count: int
  ; tokens_consumed: float
  ; last_called: Mtime.span option
}

type client_data = {
    calls: call_record Queue.t
  ; mutable last_called: Mtime.span option
  ; mutable rate_limiter: Rate_limit.t option
  ; lock: Mutex.t
}

type t = {table: client_data Client_table.t; window: Mtime.span}

(* 1 hour in nanoseconds *)
let default_window = Mtime.Span.of_uint64_ns 3_600_000_000_000L

let create ?(window = default_window) () =
  {table= Client_table.create (); window}

let make_client_data ?rate_limiter () =
  {
    calls= Queue.create ()
  ; last_called= None
  ; rate_limiter
  ; lock= Mutex.create ()
  }

(** Remove call records older than [window] from the front of [calls]. *)
let prune_calls ~window calls =
  let now_ns = Mtime.Span.to_uint64_ns (Mtime_clock.elapsed ()) in
  let window_ns = Mtime.Span.to_uint64_ns window in
  if Int64.compare now_ns window_ns > 0 then
    let cutoff = Mtime.Span.of_uint64_ns (Int64.sub now_ns window_ns) in
    let rec loop () =
      match Queue.peek_opt calls with
      | Some record when Mtime.Span.compare record.timestamp cutoff < 0 ->
          ignore (Queue.pop calls) ;
          loop ()
      | _ ->
          ()
    in
    loop ()

let add_client t ~client_id ?rate_limiter () =
  Client_table.insert t.table ~client_id (make_client_data ?rate_limiter ())

let remove_client t ~client_id =
  let rl_opt =
    match Client_table.get_exact t.table ~client_id with
    | Some data ->
        with_lock data.lock (fun () ->
            let rl = data.rate_limiter in
            data.rate_limiter <- None ;
            rl
        )
    | None ->
        None
  in
  Client_table.delete t.table ~client_id ;
  Option.iter Rate_limit.delete rl_opt

let mem t ~client_id = Client_table.mem t.table ~client_id

let get_stats t ~client_id =
  match Client_table.get t.table ~client_id with
  | Some data ->
      with_lock data.lock (fun () ->
          prune_calls ~window:t.window data.calls ;
          let call_count = Queue.length data.calls in
          let tokens_consumed =
            Queue.fold (fun acc r -> acc +. r.amount) 0.0 data.calls
          in
          Some {call_count; tokens_consumed; last_called= data.last_called}
      )
  | None ->
      None

let set_rate_limiter t ~client_id rate_limiter =
  match Client_table.get_exact t.table ~client_id with
  | Some data ->
      let old_rl =
        with_lock data.lock (fun () ->
            let old = data.rate_limiter in
            data.rate_limiter <- Some rate_limiter ;
            old
        )
      in
      ( match old_rl with
      | Some rl when rl != rate_limiter ->
          Rate_limit.delete rl
      | _ ->
          ()
      ) ;
      true
  | None ->
      false

let remove_rate_limiter t ~client_id =
  let rl_opt =
    match Client_table.get_exact t.table ~client_id with
    | Some data ->
        with_lock data.lock (fun () ->
            let rl = data.rate_limiter in
            data.rate_limiter <- None ;
            rl
        )
    | None ->
        None
  in
  Option.iter Rate_limit.delete rl_opt

(** Look up a client by wildcard matching; if not found, auto-add with
    the exact [client_id] and no rate limiter. Returns [None] only when
    the key is all-wildcard (rejected by insert). *)
let get_or_create t ~client_id =
  match Client_table.get t.table ~client_id with
  | Some data ->
      Some data
  | None ->
      let data = make_client_data () in
      if Client_table.insert t.table ~client_id data then
        Some data
      else
        (* Race: another thread inserted first, or key was all-wildcard *)
        Client_table.get t.table ~client_id

let record_and_get_limiter ~window data amount =
  with_lock data.lock (fun () ->
      let now = Mtime_clock.elapsed () in
      Queue.push {timestamp= now; amount} data.calls ;
      data.last_called <- Some now ;
      prune_calls ~window data.calls ;
      data.rate_limiter
  )

let submit_async t ~client_id ~callback amount =
  match get_or_create t ~client_id with
  | Some data -> (
    match record_and_get_limiter ~window:t.window data amount with
    | Some rl ->
        Rate_limit.submit_async rl ~callback amount
    | None ->
        callback ()
  )
  | None ->
      callback ()

let submit_sync t ~client_id ~callback amount =
  match get_or_create t ~client_id with
  | Some data -> (
    match record_and_get_limiter ~window:t.window data amount with
    | Some rl ->
        Rate_limit.submit_sync rl ~callback amount
    | None ->
        callback ()
  )
  | None ->
      callback ()
