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

(** Key type for table lookups.
    A lone [*] is a full wildcard and a trailing [*] indicates prefix matching. *)
module Key : sig
  type t = {user_agent: string; client_ip: string}

  val equal : t -> t -> bool

  val matches : pattern:t -> target:t -> bool
  (** [matches ~pattern ~target] returns true if [pattern] matches [target].
      A lone [*] in [pattern] matches any value and a trailing [*] in [pattern]
      performs prefix matching. Other fields must match exactly. *)

  val compare : t -> t -> int
  (** Total order: fewer wildcards first, then lexicographic by fields. *)
end

(** List of entries mapping keys to values.
    Lookups use wildcard matching with priority: exact > prefix > full wildcard. *)
type 'a t

val create : unit -> 'a t
(** [create ()] creates a new empty table. *)

val insert : 'a t -> caller_id:Key.t -> 'a -> bool
(** [insert t ~caller_id data] adds an entry for the given caller_id.
    Returns [false] if an entry already exists for that exact key, or if
    caller_id has both fields as [*] (all-wildcard keys are rejected). *)

val mem : 'a t -> caller_id:Key.t -> bool
(** [mem t ~caller_id] returns whether [caller_id] matches any entry
    in the table using wildcard matching. *)

val delete : 'a t -> caller_id:Key.t -> unit
(** [delete t ~caller_id] removes the entry for the caller_id. *)

val get : 'a t -> caller_id:Key.t -> 'a list
(** [get t ~caller_id] returns the values for all entries whose key matches
    [caller_id], ordered from most specific to least specific match
    (exact > prefix > full wildcard). Returns the empty list if no entry
    matches. *)

val get_exact : 'a t -> caller_id:Key.t -> 'a option
(** [get_exact t ~caller_id] returns the value for the entry whose key
    is exactly equal to [caller_id], or [None]. Does not use wildcard
    matching. *)
