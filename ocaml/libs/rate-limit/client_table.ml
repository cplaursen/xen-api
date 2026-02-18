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

module D = Debug.Make (struct let name = "client_table" end)

module Key = struct
  type t = {user_agent: string; host_ip: string}

  let equal a b = a.user_agent = b.user_agent && a.host_ip = b.host_ip

  (** Empty string acts as wildcard, matching any value *)
  let matches ~pattern ~target =
    (pattern.user_agent = "" || pattern.user_agent = target.user_agent)
    && (pattern.host_ip = "" || pattern.host_ip = target.host_ip)

  (** Priority for matching: exact (0) > host_ip only (1) > user_agent only (2) *)
  let compare_wildcard k =
    ( if k.user_agent = "" then
        2
      else
        0
    )
    +
    if k.host_ip = "" then
      1
    else
      0

  let is_all_wildcard k = k.user_agent = "" && k.host_ip = ""

  (** Total order: fewer wildcards first, then lexicographic by fields *)
  let compare a b =
    match compare (compare_wildcard a) (compare_wildcard b) with
    | 0 -> (
      match String.compare a.user_agent b.user_agent with
      | 0 ->
          String.compare a.host_ip b.host_ip
      | n ->
          n
    )
    | n ->
        n
end

type 'a cached_table = {
    table: (Key.t * 'a) list
  ; cache: (Key.t, 'a option) Lru.t
}

type 'a t = 'a cached_table Atomic.t

let create () = Atomic.make {table= []; cache= Lru.create 100}

(** Find the best matching entry for a client_id.
    List is pre-sorted by Key.compare (most specific first), so first match wins.
    Priority: exact match > host_ip specified > user_agent specified *)
let find_match {table; cache} ~client_id =
  let entry_opt = Lru.lookup cache client_id in
  match entry_opt with
  | Some result ->
      result
  | None ->
      let result =
        Option.map snd
          (List.find_opt
             (fun (key, _) -> Key.matches ~pattern:key ~target:client_id)
             table
          )
      in
      Lru.add_trim cache client_id result ;
      result

let mem t ~client_id =
  let entries = Atomic.get t in
  Option.is_some (find_match entries ~client_id)

(* TODO: Indicate failure reason - did we get invalid config or try to add an
   already present client_id? *)
let insert t ~client_id data =
  if Key.is_all_wildcard client_id then
    false
  (* Reject keys with both fields empty *)
  else
    let {table; _} = Atomic.get t in
    if List.exists (fun (key, _) -> Key.equal key client_id) table then
      false
    else (
      Atomic.set t
        {
          table=
            List.sort
              (fun (k1, _) (k2, _) -> Key.compare k1 k2)
              ((client_id, data) :: table)
        ; cache= Lru.create 100
        } ;
      true
    )

let delete t ~client_id =
  let {table; _} = Atomic.get t in
  match List.find_opt (fun (key, _) -> Key.equal key client_id) table with
  | None ->
      ()
  | Some _ ->
      Atomic.set t
        {
          table=
            List.filter (fun (key, _) -> not (Key.equal key client_id)) table
        ; cache= Lru.create 100
        }

let get t ~client_id =
  let entries = Atomic.get t in
  find_match entries ~client_id

let get_exact t ~client_id =
  let {table; _} = Atomic.get t in
  Option.map snd (List.find_opt (fun (key, _) -> Key.equal key client_id) table)
