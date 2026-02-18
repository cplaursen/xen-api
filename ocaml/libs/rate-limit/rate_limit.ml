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

module D = Debug.Make (struct let name = "rate_limit" end)

type t = {
    bucket: Token_bucket.t
  ; process_queue:
      (float * (unit -> unit)) Queue.t (* contains token cost and callback *)
  ; process_queue_lock: Mutex.t
  ; worker_thread_cond: Condition.t
  ; should_terminate: bool ref (* signal termination to worker thread *)
  ; worker_thread: Thread.t
}

let with_lock = Xapi_stdext_threads.Threadext.Mutex.execute

(* The worker thread is responsible for calling the callback when the token
   amount becomes available *)
let rec worker_loop ~bucket ~process_queue ~process_queue_lock
    ~worker_thread_cond ~should_terminate =
  let process_item cost callback =
    Token_bucket.delay_then_consume bucket cost ;
    callback ()
  in
  let item_opt =
    with_lock process_queue_lock (fun () ->
        while Queue.is_empty process_queue && not !should_terminate do
          Condition.wait worker_thread_cond process_queue_lock
        done ;
        Queue.take_opt process_queue
    )
  in
  match item_opt with
  | None ->
      (* Queue is empty only when termination was signalled *)
      ()
  | Some (cost, callback) ->
      process_item cost callback ;
      worker_loop ~bucket ~process_queue ~process_queue_lock ~worker_thread_cond
        ~should_terminate

let create ~burst_size ~fill_rate =
  match Token_bucket.create ~burst_size ~fill_rate with
  | Some bucket ->
      let process_queue = Queue.create () in
      let process_queue_lock = Mutex.create () in
      let worker_thread_cond = Condition.create () in
      let should_terminate = ref false in
      let worker_thread =
        Thread.create
          (fun () ->
            worker_loop ~bucket ~process_queue ~process_queue_lock
              ~worker_thread_cond ~should_terminate
          )
          ()
      in
      {
        bucket
      ; process_queue
      ; process_queue_lock
      ; worker_thread_cond
      ; should_terminate
      ; worker_thread
      }
  | None ->
      raise (Failure "Invalid token bucket parameters")

let delete data =
  with_lock data.process_queue_lock (fun () ->
      data.should_terminate := true ;
      Condition.signal data.worker_thread_cond
  ) ;
  Thread.join data.worker_thread

(* The callback should return quickly - if it is a longer task it is
   responsible for creating a thread to do the task *)
let submit_async
    ({bucket; process_queue; process_queue_lock; worker_thread_cond; _} as _data)
    ~callback amount =
  let run_immediately =
    with_lock process_queue_lock (fun () ->
        let immediate =
          Queue.is_empty process_queue && Token_bucket.consume bucket amount
        in
        if not immediate then (
          Queue.add (amount, callback) process_queue ;
          Condition.signal worker_thread_cond
        ) ;
        immediate
    )
  in
  if run_immediately then
    callback ()
  else
    D.debug "rate limiting sync call"

(* Block and execute on the same thread *)
let submit_sync bucket_data ~callback amount =
  let channel_opt =
    with_lock bucket_data.process_queue_lock (fun () ->
        if
          Queue.is_empty bucket_data.process_queue
          && Token_bucket.consume bucket_data.bucket amount
        then
          None
        (* Can run callback immediately after releasing lock *)
        else
          (* Rate limited, need to retrieve function result via channel *)
          let channel = Event.new_channel () in
          Queue.add
            (amount, fun () -> Event.sync (Event.send channel ()))
            bucket_data.process_queue ;
          Condition.signal bucket_data.worker_thread_cond ;
          Some channel
    )
  in
  match channel_opt with
  | None ->
      callback ()
  | Some channel ->
      Event.sync (Event.receive channel) ;
      callback ()
