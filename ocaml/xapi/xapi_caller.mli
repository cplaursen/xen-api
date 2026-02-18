(*
 * Copyright (C) Citrix Systems Inc.
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

module Key = Client_table.Key

val tracker : Client_tracker.t

val submit_sync : client_id:Key.t -> callback:(unit -> 'a) -> float -> 'a

val submit : client_id:Key.t -> callback:(unit -> unit) -> float -> unit

val get_stats : client_id:Key.t -> Client_tracker.stats option

val get_token_cost : string -> float

val default_token_cost : float

val create :
     __context:Context.t
  -> name_label:string
  -> user_agent:string
  -> host_ip:string
  -> [`Caller] Ref.t

val destroy : __context:Context.t -> self:[`Caller] API.Ref.t -> unit

val register : __context:Context.t -> unit
(** Load caller records from the database into the client tracker *)
