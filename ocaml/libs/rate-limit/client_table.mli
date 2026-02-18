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

(** Key type for table lookups. Empty strings act as wildcards. *)
module Key : sig
  type t = {user_agent: string; host_ip: string}

  val equal : t -> t -> bool

  val matches : pattern:t -> target:t -> bool
  (** [matches ~pattern ~target] returns true if [pattern] matches [target].
      Empty strings in [pattern] act as wildcards matching any value. *)

  val compare : t -> t -> int
  (** Total order: fewer wildcards first, then lexicographic by fields. *)
end

(** List of entries mapping keys to values.
    Lookups use wildcard matching with priority: exact > host_ip only > user_agent only. *)
type 'a t

val create : unit -> 'a t
(** [create ()] creates a new empty table. *)

val insert : 'a t -> client_id:Key.t -> 'a -> bool
(** [insert t ~client_id data] adds an entry for the given client_id.
    Returns [false] if an entry already exists for that exact key, or if
    client_id has both fields empty (all-wildcard keys are rejected). *)

val mem : 'a t -> client_id:Key.t -> bool
(** [mem t ~client_id] returns whether [client_id] matches any entry
    in the table using wildcard matching. *)

val delete : 'a t -> client_id:Key.t -> unit
(** [delete t ~client_id] removes the entry for the client_id. *)

val get : 'a t -> client_id:Key.t -> 'a option
(** [get t ~client_id] returns the value for the best matching entry,
    or [None] if no entry matches. Uses wildcard matching with priority:
    exact > host_ip only > user_agent only. *)

val get_exact : 'a t -> client_id:Key.t -> 'a option
(** [get_exact t ~client_id] returns the value for the entry whose key
    is exactly equal to [client_id], or [None]. Does not use wildcard
    matching. *)
