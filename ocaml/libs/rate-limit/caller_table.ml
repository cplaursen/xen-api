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

module D = Debug.Make (struct let name = "caller_table" end)

module Key = struct
  type t = {user_agent: string; client_ip: string}

  let equal a b = a.user_agent = b.user_agent && a.client_ip = b.client_ip

  let prefix_matches ~prefix target =
    let prefix_len = String.length prefix in
    String.length target >= prefix_len
    && String.equal (String.sub target 0 prefix_len) prefix

  (** A lone [*] matches everything. A trailing [*] indicates prefix matching.
      Otherwise the field must match exactly. *)
  let field_matches ~pattern ~target =
    let pattern_len = String.length pattern in
    if pattern = "*" then
      true
    else if pattern_len > 1 && pattern.[pattern_len - 1] = '*' then
      prefix_matches ~prefix:(String.sub pattern 0 (pattern_len - 1)) target
    else
      pattern = target

  let matches ~pattern ~target =
    field_matches ~pattern:pattern.user_agent ~target:target.user_agent
    && field_matches ~pattern:pattern.client_ip ~target:target.client_ip

  let wildcard_score field =
    let field_len = String.length field in
    if field = "*" then
      2
    else if field_len > 1 && field.[field_len - 1] = '*' then
      1
    else
      0

  let compare_wildcard k =
    let user_agent_score = wildcard_score k.user_agent in
    let client_ip_score = wildcard_score k.client_ip in
    (user_agent_score + client_ip_score, user_agent_score, client_ip_score)

  let is_all_wildcard k = k.user_agent = "*" && k.client_ip = "*"

  (** Total order: fewer wildcards first, then lexicographic by fields *)
  let compare a b =
    match compare (compare_wildcard a) (compare_wildcard b) with
    | 0 -> (
      match String.compare a.user_agent b.user_agent with
      | 0 ->
          String.compare a.client_ip b.client_ip
      | n ->
          n
    )
    | n ->
        n
end

type 'a cached_table = {table: (Key.t * 'a) list; cache: (Key.t, 'a list) Lru.t}

type 'a t = 'a cached_table Atomic.t

let cache_capacity = 100

let create () = Atomic.make {table= []; cache= Lru.create cache_capacity}

(** Build a fresh cache from [old_cache] but drop entries whose cached
    target is matched by [pattern]. Those are exactly the entries whose
    result list would change if [pattern] is inserted into or removed
    from the table. *)
let cache_without_matches ~pattern old_cache =
  Lru.filter old_cache ~f:(fun target _ -> not (Key.matches ~pattern ~target))

(** Insert [entry] into [table] (sorted by Key.compare ascending, i.e. most
    specific first) at the position that preserves the ordering. *)
let rec insert_sorted entry table =
  match table with
  | [] ->
      [entry]
  | (k, _) :: _ when Key.compare (fst entry) k <= 0 ->
      entry :: table
  | hd :: tl ->
      hd :: insert_sorted entry tl

(** Find all matching entries for a caller_id, ordered by Key.compare
    (most specific first). Priority: exact > prefix > full wildcard. *)
let find_matches {table; cache} ~caller_id =
  let entry_opt = Lru.lookup cache caller_id in
  match entry_opt with
  | Some result ->
      result
  | None ->
      let result =
        List.filter_map
          (fun (key, v) ->
            if Key.matches ~pattern:key ~target:caller_id then
              Some v
            else
              None
          )
          table
      in
      Lru.add_trim cache caller_id result ;
      result

let mem t ~caller_id =
  let entries = Atomic.get t in
  find_matches entries ~caller_id <> []

let insert t ~caller_id data =
  if Key.is_all_wildcard caller_id then
    false
  (* Reject keys with both fields full wildcards. *)
  else
    let {table; cache} = Atomic.get t in
    if List.exists (fun (key, _) -> Key.equal key caller_id) table then
      false
    else (
      Atomic.set t
        {
          table= insert_sorted (caller_id, data) table
        ; cache= cache_without_matches ~pattern:caller_id cache
        } ;
      true
    )

let delete t ~caller_id =
  let {table; cache} = Atomic.get t in
  match List.find_opt (fun (key, _) -> Key.equal key caller_id) table with
  | None ->
      ()
  | Some _ ->
      Atomic.set t
        {
          table=
            List.filter (fun (key, _) -> not (Key.equal key caller_id)) table
        ; cache= cache_without_matches ~pattern:caller_id cache
        }

let get t ~caller_id =
  let entries = Atomic.get t in
  find_matches entries ~caller_id

let get_exact t ~caller_id =
  let {table; _} = Atomic.get t in
  Option.map snd (List.find_opt (fun (key, _) -> Key.equal key caller_id) table)
