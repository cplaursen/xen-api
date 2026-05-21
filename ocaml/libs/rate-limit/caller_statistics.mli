type t

val create : caller_uuid:string -> t
(** [create ~caller_uuid] creates a fresh statistics record with zero counts
    and a [last_called] of [Mtime.Span.zero]. *)

val register_call : token_amount:float -> t -> unit
(** Track that a client has made a call *)

val register_call_timestamp :
  token_amount:float -> timestamp:Mtime.span -> t -> unit
(** Same as [register_call] but uses an explicit timestamp. Exposed for testing. *)

val get_uuid : t -> string

val get_last_called : t -> Mtime.span

val get_call_count : t -> int

val get_token_count : t -> float
