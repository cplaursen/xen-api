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

(** A client tracker built on top of {!Client_table}. Tracks per-client
    statistics (call volume, tokens consumed, last call time) within a
    configurable sliding window, and supports optional per-client rate
    limiting via {!Rate_limit}.

    Clients are automatically added on first submission if no matching
    entry exists. Statistics are maintained over a sliding time window
    (default: 1 hour).

    Management operations ({!add_client}, {!remove_client},
    {!set_rate_limiter}, {!remove_rate_limiter}) use exact key matching.
    Query and submission operations ({!mem}, {!get_stats},
    {!submit_async}, {!submit_sync}) use wildcard matching. *)

(** Per-client statistics within the sliding window. [last_called] is the
    absolute time of the most recent call (not affected by window
    pruning). *)
type stats = {
    call_count: int
  ; tokens_consumed: float
  ; last_called: Mtime.span option
}

type t

val create : ?window:Mtime.span -> unit -> t
(** [create ?window ()] creates a new empty client tracker. [window]
    controls how far back call statistics are retained (default: 1
    hour). *)

val add_client :
     t
  -> client_id:Client_table.Key.t
  -> ?rate_limiter:Rate_limit.t
  -> unit
  -> bool
(** [add_client t ~client_id ?rate_limiter ()] registers a new client.
    Returns [false] if the client already exists or the key is
    all-wildcard. *)

val remove_client : t -> client_id:Client_table.Key.t -> unit
(** [remove_client t ~client_id] removes the client (exact key match) and
    deletes any attached rate limiter. *)

val mem : t -> client_id:Client_table.Key.t -> bool
(** [mem t ~client_id] returns whether a matching client exists
    (wildcard matching). *)

val get_stats : t -> client_id:Client_table.Key.t -> stats option
(** [get_stats t ~client_id] returns the statistics for the best matching
    client, or [None] if no client matches (wildcard matching).
    Statistics are pruned to the sliding window before returning. *)

val set_rate_limiter : t -> client_id:Client_table.Key.t -> Rate_limit.t -> bool
(** [set_rate_limiter t ~client_id rl] attaches a rate limiter to the
    client identified by exact key match. Any previously attached rate
    limiter is deleted first. Returns [false] if no client was found. *)

val remove_rate_limiter : t -> client_id:Client_table.Key.t -> unit
(** [remove_rate_limiter t ~client_id] removes and deletes the rate
    limiter from the client (exact key match), if any. *)

val submit_async :
  t -> client_id:Client_table.Key.t -> callback:(unit -> unit) -> float -> unit
(** [submit_async t ~client_id ~callback amount] records the call and
    submits the callback. If the matching client has a rate limiter, the
    callback is subject to rate limiting; otherwise it runs immediately.
    If no matching client is found, one is automatically added with no
    rate limiter. Uses wildcard matching. *)

val submit_sync :
  t -> client_id:Client_table.Key.t -> callback:(unit -> 'a) -> float -> 'a
(** [submit_sync t ~client_id ~callback amount] records the call and
    submits the callback, blocking until it completes. If the matching
    client has a rate limiter, the callback is subject to rate limiting;
    otherwise it runs immediately. If no matching client is found, one is
    automatically added with no rate limiter. Uses wildcard matching. *)
