(*
 * Copyright (C) Cloud Software Group
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

open Datamodel_types
open Datamodel_common
open Datamodel_roles

let lifecycle = []

let create =
  call ~name:"create" ~lifecycle
    ~params:
      [
        ( String
        , "name_label"
        , "Label for this user. For identification purposes"
        )
      ; ( String
        , "user_agent"
        , "User agent of the caller. Set to the empty string to match any user \
           agent."
        )
      ; ( String
        , "host_ip"
        , "IP address of the caller. Set to empty string to match all \
           addresses."
        )
      ]
    ~doc:"Register a new tracked caller" ~allowed_roles:_R_POOL_OP ()
    ~result:(Ref _caller, "The reference of the created rate limit.")

let destroy =
  call ~name:"destroy" ~lifecycle
    ~params:[(Ref _caller, "self", "The caller to destroy")]
    ~doc:"Destroy a caller" ~allowed_roles:_R_POOL_OP ()

let t =
  create_obj ~name:_caller ~descr:"XAPI caller description and rate limiting"
    ~doccomments:[] ~gen_constructor_destructor:false ~gen_events:true
    ~in_db:true ~lifecycle ~persist:PersistEverything ~in_oss_since:None
    ~messages_default_allowed_roles:_R_POOL_ADMIN
    ~contents:
      ([uid _caller ~lifecycle]
      @ [
          field ~qualifier:RW ~ty:String ~lifecycle "name_label"
            "Name label for the caller" ~ignore_foreign_key:true
            ~default_value:(Some (VString ""))
        ; field ~qualifier:StaticRO ~ty:String ~lifecycle "user_agent"
            "User agent of the caller" ~ignore_foreign_key:true
            ~default_value:(Some (VString ""))
        ; field ~qualifier:StaticRO ~ty:String ~lifecycle "host_ip"
            "IP address of the caller" ~ignore_foreign_key:true
            ~default_value:(Some (VString ""))
        ; field ~qualifier:StaticRO ~ty:Float ~lifecycle "burst_size"
            "Amount of tokens that can be consumed at once"
            ~ignore_foreign_key:true ~default_value:(Some (VFloat (-1.)))
        ; field ~qualifier:StaticRO ~ty:Float ~lifecycle "fill_rate"
            "Tokens added to token bucket per second" ~ignore_foreign_key:true
            ~default_value:(Some (VFloat (-1.)))
        ]
      )
    ~messages:[create; destroy] ()
