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
module D = Debug.Make (struct let name = "xapi_client" end)

module Key = Client_table.Key

let tracker = Client_tracker.create ()

let submit_sync ~client_id ~callback amount =
  Client_tracker.submit_sync tracker ~client_id ~callback amount

let submit ~client_id ~callback amount =
  Client_tracker.submit_async tracker ~client_id ~callback amount

let get_stats ~client_id = Client_tracker.get_stats tracker ~client_id

let create ~__context ~name_label ~user_agent ~host_ip =
  if user_agent = "" && host_ip = "" then
    raise
      Api_errors.(
        Server_error
          (invalid_value, ["Expected user_agent or host_ip to be nonempty"])
      ) ;
  let client_id = Key.{user_agent; host_ip} in
  if Client_tracker.mem tracker ~client_id then
    raise
      Api_errors.(
        Server_error
          ( map_duplicate_key
          , ["user_agent"; user_agent; "user_agent already registered"]
          )
      ) ;
  let uuid = Uuidx.make () in
  let ref = Ref.make () in
  if Client_tracker.add_client tracker ~client_id () then (
    Db.Caller.create ~__context ~ref ~uuid:(Uuidx.to_string uuid) ~user_agent
      ~host_ip ~burst_size:(-1.) ~fill_rate:(-1.) ~name_label ;
    ref
  ) else
    raise
      Api_errors.(
        Server_error (internal_error, ["Failed to add client to tracker"])
      )

let destroy ~__context ~self =
  let record = Db.Caller.get_record ~__context ~self in
  let client_id =
    Key.{user_agent= record.caller_user_agent; host_ip= record.caller_host_ip}
  in
  Client_tracker.remove_client tracker ~client_id ;
  Db.Caller.destroy ~__context ~self

let register ~__context =
  List.iter
    (fun self ->
      let record = Db.Caller.get_record ~__context ~self in
      let client_id =
        Key.
          {
            user_agent= record.API.caller_user_agent
          ; host_ip= record.API.caller_host_ip
          }
      in
      ignore (Client_tracker.add_client tracker ~client_id ())
    )
    (Db.Caller.get_all ~__context)

(* These are the average times taken by xapi to fulfil the requests *)
let token_costs =
  Hashtbl.of_seq
    (List.to_seq
       [
         ("VDI.pool_migrate", 250.)
       ; ("VM.migrate_send", 200.)
       ; ("VM.suspend", 40.)
       ; ("VM.resume_on", 40.)
       ; ("SR.probe", 40.)
       ; ("VM.copy", 30.)
       ; ("pool.enable_ha", 30.)
       ; ("VM.checkpoint", 20.)
       ; ("host.ha_join_liveset", 20.)
       ; ("Cluster.pool_create", 20.)
       ; ("VDI.copy", 20.)
       ; ("VM.pool_migrate", 20.)
       ; ("VM.resume", 20.)
       ; ("SR.destroy", 20.)
       ; ("Cluster_host.create", 15.)
       ; ("event.from", 15.)
       ; ("pool.management_reconfigure", 10.)
       ; ("pool.join", 10.)
       ; ("pool.disable_ha", 10.)
       ; ("host.prepare_for_poweroff", 10.)
       ; ("VM.set_memory_dynamic_range", 10.)
       ; ("host.evacuate", 7.5)
       ; ("VM.clean_reboot", 7.)
       ; ("VM.restart_device_models", 7.)
       ; ("pool_update.apply", 7.)
       ; ("Bond.create", 6.)
       ; ("VM.clean_shutdown", 6.)
       ; ("VM.revert", 5.)
       ; ("host.install_server_certificate", 5.)
       ; ("pool.eject", 4.)
       ; ("Cluster.create", 4.)
       ; ("pool.sync_updates", 4.)
       ; ("host.apply_updates", 4.)
       ; ("SR.probe_ext", 4.)
       ; ("host.ha_wait_for_shutdown_via_statefile", 4.)
       ; ("pool_update.precheck", 3.)
       ; ("event.next", 3.)
       ; ("VDI.snapshot", 3.)
       ; ("pool_update.introduce", 3.)
       ; ("pool.enable_external_auth", 2.)
       ; ("VM.start_on", 2.)
       ; ("VM.hard_reboot", 2.)
       ; ("SR.create", 2.)
       ; ("VM.hard_shutdown", 2.)
       ; ("pool.designate_new_master", 2.)
       ; ("VM.start", 2.)
       ; ("VDI.clone", 2.)
       ; ("host.ha_release_resources", 1.5)
       ; ("VM.snapshot", 1.5)
       ; ("pool.is_slave", 1.5)
       ; ("pool.recover_slaves", 1.5)
       ; ("host.preconfigure_ha", 1.5)
       ; ("pool_update.detach", 1.5)
       ; ("pool_update.attach", 1.5)
       ; ("host.update_master", 1.5)
       ; ("PBD.plug", 1.5)
       ; ("Repository.apply", 1.2)
       ; ("pool.emergency_reset_master", 1.2)
       ; ("VBD.plug", 1.2)
       ; ("host.commit_new_master", 1.2)
       ; ("SR.scan", 1.)
       ; ("VBD.unplug", 1.)
       ; ("pool_update.pool_clean", 1.)
       ; ("VM.clone", 1.)
       ; ("VM.provision", 1.)
       ; ("PIF.reconfigure_ip", 1.)
       ; ("pool.create_VLAN_from_PIF", 0.8)
       ; ("pool.apply_edition", 0.8)
       ; ("pool.disable_external_auth", 0.7)
       ; ("VM.pool_migrate_complete", 0.7)
       ; ("host.call_plugin", 0.7)
       ; ("VLAN.create", 0.6)
       ; ("VDI.create", 0.6)
       ; ("host.update_firewalld_service_status", 0.6)
       ; ("VDI.destroy", 0.5)
       ; ("VIF.plug", 0.5)
       ; ("host.set_iscsi_iqn", 0.5)
       ; ("SR.update", 0.5)
       ; ("VDI.resize", 0.5)
       ; ("host.management_reconfigure", 0.4)
       ; ("VIF.unplug", 0.3)
       ; ("host.set_https_only", 0.3)
       ; ("PIF.plug", 0.3)
       ; ("host.disable_external_auth", 0.3)
       ; ("VDI.set_name_label", 0.3)
       ; ("VDI.set_name_description", 0.3)
       ; ("PIF.scan", 0.3)
       ]
    )

let default_token_cost = 0.1

let get_token_cost name =
  let amount = Hashtbl.find_opt token_costs name in
  Option.value ~default:default_token_cost amount
