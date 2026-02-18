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
module D = Debug.Make (struct let name = "xapi_rate_limit" end)

module Key = Client_table.Key

let client_id_of_caller ~__context ~caller =
  let record = Db.Caller.get_record ~__context ~self:caller in
  Key.{user_agent= record.caller_user_agent; host_ip= record.caller_host_ip}

let create ~__context ~caller ~burst_size ~fill_rate =
  let client_id = client_id_of_caller ~__context ~caller in
  (* Ensure the client exists in the tracker *)
  ignore (Client_tracker.add_client Xapi_caller.tracker ~client_id ()) ;
  let rl =
    try Rate_limit.create ~burst_size ~fill_rate
    with Failure _ ->
      raise
        Api_errors.(
          Server_error
            ( invalid_value
            , [
                "fill_rate"
              ; string_of_float fill_rate
              ; "Fill rate must be positive"
              ]
            )
        )
  in
  if not (Client_tracker.set_rate_limiter Xapi_caller.tracker ~client_id rl)
  then (
    Rate_limit.delete rl ;
    raise
      Api_errors.(
        Server_error (internal_error, ["Failed to attach rate limiter"])
      )
  ) ;
  let uuid = Uuidx.make () in
  let ref = Ref.make () in
  Db.Rate_limit.create ~__context ~ref ~uuid:(Uuidx.to_string uuid) ~caller
    ~burst_size ~fill_rate ;
  ref

let destroy ~__context ~self =
  let record = Db.Rate_limit.get_record ~__context ~self in
  let client_id =
    client_id_of_caller ~__context ~caller:record.rate_limit_caller
  in
  Client_tracker.remove_rate_limiter Xapi_caller.tracker ~client_id ;
  Db.Rate_limit.destroy ~__context ~self

let register ~__context =
  List.iter
    (fun self ->
      let rl_record = Db.Rate_limit.get_record ~__context ~self in
      let client_id =
        client_id_of_caller ~__context ~caller:rl_record.API.rate_limit_caller
      in
      let burst_size = rl_record.API.rate_limit_burst_size in
      let fill_rate = rl_record.API.rate_limit_fill_rate in
      if burst_size > 0. && fill_rate > 0. then (
        ignore (Client_tracker.add_client Xapi_caller.tracker ~client_id ()) ;
        try
          let rl = Rate_limit.create ~burst_size ~fill_rate in
          ignore
            (Client_tracker.set_rate_limiter Xapi_caller.tracker ~client_id rl)
        with Failure msg ->
          D.warn "Failed to create rate limiter for %s/%s: %s"
            client_id.user_agent client_id.host_ip msg
      )
    )
    (Db.Rate_limit.get_all ~__context)
