type 'a node = {
    value: 'a
  ; mutable prev: 'a node option
  ; mutable next: 'a node option
}

type 'a t = {mutable first: 'a node option; mutable last: 'a node option}

let create () = {first= None; last= None}

let node x = {value= x; prev= None; next= None}

let append t n =
  match t.last with
  | None ->
      let node = Some n in
      t.first <- node ;
      t.last <- node
  | Some lst ->
      let node = Some n in
      lst.next <- node ;
      n.prev <- t.last ;
      t.last <- node

(** [drop] a node [n] from (its) list [t]. The interesting property is
    that we can drop any element from its list that we know. However,
    we don't check that [n] is indeed a member of [t] and it's an
    unchecked error to pass an [n] that is not a member of [t].

    This is similar to a
    pointer-based implementation in C. We infer that we need to update
    the fist, last entry of the list of [n]'s prev or next is [None],
    hence it is the first or last element in the list. *)
let drop t n =
  let np = n.prev in
  let nn = n.next in
  ( match np with
  | None ->
      t.first <- nn
  | Some x ->
      x.next <- nn ;
      n.prev <- None
  ) ;
  match nn with
  | None ->
      t.last <- np
  | Some x ->
      x.prev <- np ;
      n.next <- None

let first t = t.first

let last t = t.last

let value node = node.value

let foldl f zero t =
  let rec loop acc = function
    | None ->
        acc
    | Some n ->
        loop (f acc n.value) n.next
  in
  loop zero t.first

let foldr f t zero =
  let rec loop acc = function
    | None ->
        acc
    | Some n ->
        loop (f n.value acc) n.prev
  in
  loop zero t.last

let to_list t = foldr (fun x xs -> x :: xs) t []

let from_list xs =
  let t = create () in
  List.iter (fun x -> append t (node x)) xs ;
  t

(*
 * Copyright (C) 2023 Cloud Software Group
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
