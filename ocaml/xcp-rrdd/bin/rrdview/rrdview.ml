(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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

open Bos_setup

type def = Def of string * Rrd.cf_type | Cdef of string

let name ~ds_name ~cf_type =
  cf_type
  |> Rrd.cf_type_to_string
  |> String.Ascii.lowercase
  |> Printf.sprintf "%s_%s" ds_name

type ds_def = {units: string option}

let default_def = {units= None}

let def ~data ~step ~ds_name ~cf_type =
  let cfstr = Rrd.cf_type_to_string cf_type in
  let namestr = name ~ds_name ~cf_type in
  ( Def (ds_name, cf_type)
  , Printf.sprintf "DEF:%s=%s:%s:%s:step=%Ld" namestr (Fpath.to_string data)
      ds_name cfstr step
  )

type ds = Ds : string -> ds

type cdef = Op of cdef * string * cdef | Var of def

let rec string_of_cdef = function
  | Op (lhs, op, rhs) ->
      String.concat ~sep:"," [string_of_cdef lhs; string_of_cdef rhs; op]
  | Var (Def (ds_name, cf_type)) ->
      name ~ds_name ~cf_type
  | Var (Cdef s) ->
      s

let cdef name ops =
  (Cdef name, Printf.sprintf "CDEF:%s=%s" name @@ string_of_cdef ops)

type rgb = {r: int; g: int; b: int; alpha: int option}

type fill = RGB of rgb

let shape ?(stack = false) kind ?label ~def fill =
  let defstr =
    match def with
    | Def (ds_name, cf_type) ->
        name ~ds_name ~cf_type
    | Cdef str ->
        str
  in
  let fillstr =
    match fill with
    | Some (RGB {r; g; b; alpha}) ->
        Printf.sprintf "#%02x%02x%02x%s" r g b
          (Option.fold ~none:"" ~some:(Printf.sprintf "%02u") alpha)
    | None ->
        ""
  in
  Printf.sprintf "%s:%s%s%s%s" kind defstr fillstr
    (if stack then ":STACK" else "")
    (match label with None -> "" | Some x -> ":" ^ x)

let area = shape "AREA"

let area_stack = shape ~stack:true "AREA"

let line ?label = shape ?label "LINE"

(* colors from rrdtool wiki OutlinedAreaGraph  *)
let rgb ?alpha hex =
  let r = (hex lsr 16) land 0xff
  and g = (hex lsr 8) land 0xff
  and b = hex land 0xff in
  RGB {r; g; b; alpha}

let rgb light dark = (rgb light, rgb dark)

let colors =
  [|
     rgb 0x54EC48 0x24BC14
   ; rgb 0x48C4EC 0x1598C3
   ; rgb 0xDE48EC 0xB415C7
   ; rgb 0x7648EC 0x4D18E4
   ; rgb 0xEA644A 0xCC3118
   ; rgb 0xEC9D48 0xCC7016
   ; rgb 0xECD748 0xC9B215
  |]

let get_color ~dark i =
  let RGB col_light, col_dark = colors.(i mod Array.length colors) in
  Some (if dark then col_dark else RGB {col_light with alpha= Some 50})

let rrdtool ~filename ~data title ~ds_names ~first ~last ~step ~width
    ~has_min_max =
  let graph =
    List.of_seq
      (ds_names
      |> List.mapi (fun x s -> (s, x))
      |> List.to_seq
      |> Seq.flat_map @@ fun (ds_name, i) ->
         Seq.append
           ( if has_min_max then
               let ds_min, def1 = def ~step ~data ~ds_name ~cf_type:Rrd.CF_Min
               and ds_max, def2 =
                 def ~step ~data ~ds_name ~cf_type:Rrd.CF_Max
               in
               let ds_range, cdef1 =
                 cdef (ds_name ^ "range") (Op (Var ds_max, "-", Var ds_min))
               in
               List.to_seq
                 [
                   def1
                 ; def2
                 ; cdef1
                 ; area ~def:ds_min None
                 ; area_stack ~def:ds_range @@ get_color ~dark:false i
                 ]
             else
               Seq.empty
           )
           (let ds_avg, def3 =
              def ~step ~data ~ds_name ~cf_type:Rrd.CF_Average
            in
            List.to_seq
              [def3; line ~label:ds_name ~def:ds_avg @@ get_color ~dark:true i]
           )
      )
  in
  Cmd.(
    v "rrdtool"
    % "graph"
    % "--imgformat"
    % "SVG"
    % Fpath.to_string filename
    % "--title"
    % title
    % "--width"
    % string_of_int width
    % "--height"
    % "256" (* ~4 rows *)
    % "--start"
    % Int64.to_string first
    % "--end"
    % Int64.to_string last
    %% of_list graph
  )

let prepare_plot_cmds ~filename ~data rrd =
  let open Rrd in
  let has cf rra = rra.rra_cf = cf in
  let has_min =
    Array.find_opt (has Rrd.CF_Min) rrd.rrd_rras |> Option.is_some
  in
  let has_max =
    Array.find_opt (has Rrd.CF_Max) rrd.rrd_rras |> Option.is_some
  in
  rrd.rrd_rras
  |> Array.to_seq
  |> Seq.map @@ fun rra ->
     let timespan =
       Int64.mul (Int64.of_int (rra.rra_pdp_cnt * rra.rra_row_cnt)) rrd.timestep
     in
     let start = rrd.last_updated -. Int64.to_float timespan in
     let filename =
       Fpath.add_ext (Int64.to_string timespan) filename |> Fpath.add_ext "svg"
     in
     let title =
       Fpath.rem_ext filename
       |> Fpath.basename
       |> String.cuts ~sep:"."
       |> String.concat ~sep:"<br/>"
     in
     let step = Int64.(mul (of_int rra.rra_pdp_cnt) rrd.timestep) in
     let width = 2 * rra.rra_row_cnt in
     (* 1 point = 1 CDP from the RRA *)
     (* TODO: could look up original names in original_ds *)
     rrdtool ~step ~width ~data ~filename title ~ds_names:(ds_names rrd)
       ~has_min_max:(has_min && has_max) ~first:(Int64.of_float start)
       ~last:(Int64.of_float rrd.last_updated)

let prepare_plots ?(exec = false) ~filename ~data rrd =
  let output = Fpath.set_ext ".sh" filename in
  let cmds = prepare_plot_cmds ~filename ~data rrd in
  if exec then
    cmds
    |> Seq.iter @@ fun cmd ->
       OS.Cmd.run cmd
       |> Logs.on_error_msg ~use:(fun () -> failwith "failed to run rrdtool")
  else
    cmds
    |> Seq.map Cmd.to_string
    |> List.of_seq
    |> OS.File.write_lines output
    |> Logs.on_error_msg ~use:(fun _ -> exit 2)

let finally f ~(always : unit -> unit) =
  match f () with
  | result ->
      always () ; result
  | exception e ->
      always () ; raise e

let with_input_file path f =
  if Fpath.has_ext "gz" path then
    let cmd = Cmd.(v "zcat" % p path) in
    let ic = cmd |> Cmd.to_string |> Unix.open_process_in in
    finally
      (fun () -> f ic)
      ~always:(fun () ->
        let (_ : Unix.process_status) = Unix.close_process_in ic in
        ()
      )
  else
    let ic = open_in Fpath.(to_string path) in
    finally (fun () -> f ic) ~always:(fun () -> close_in ic)

let with_input_rrd f filename =
  with_input_file filename @@ fun ic ->
  Logs.info (fun m -> m "Parsing RRD %a" Fpath.pp filename) ;
  let input = Xmlm.make_input (`Channel ic) in
  let rrd = Rrd.from_xml input in
  f ~filename rrd

(* to avoid mixing data source and filenames we use a different type here *)

let make_ds ?filename dsname =
  let dsname =
    if String.length dsname >= 20 then (
      Logs.warn (fun m ->
          m "RRD data source name exceeds 20 char limit: %s" dsname
      ) ;
      String.with_range dsname ~len:19
    ) else
      dsname
  in
  (Option.map Fpath.v filename, Ds dsname)

let make_sr (dsname, uuid) = make_ds ~filename:("_sr_" ^ uuid) dsname

let make_vbd (vbd, dsname) = make_ds ~filename:vbd dsname

let make_runstate dsname = make_ds ~filename:"runstate" dsname

(* top-level value to compile regexes only once *)
let classify =
  (* some RRD data source names are too long, max is 20 chars.
     Splitting RRDs into different files allows to shorten the names,
     e.g. remove the UUID from SR datasources.
     Some names are still too long, but those can be shortened without losing information. *)
  let open Tyre in
  let uuid8 = pcre "[0-9a-f]{8}" in
  let uuid_rest = pcre "(-[0-9a-f]{4}){3}-[0-9a-f]{12}" in
  let dsname = pcre "[a-zA-Z_]+" in
  let shorten from target = str from --> fun () -> make_ds target in
  [
    (dsname <&> char '_' *> uuid8) --> make_sr
  ; (str "sr_" *> uuid8 <* uuid_rest <* char '_' <&> dsname) --> make_sr
  ; shorten "Tapdisks_in_low_memory_mode" "Tapdisks_in_lowmem"
  ; ( (opt dsname <* str "memory_" <&> dsname) --> fun (pre, post) ->
      make_ds (Option.value ~default:"" pre ^ "mem_" ^ post)
    )
  ; (pcre "vbd_[^_]+" <* char '_' <&> dsname) --> make_vbd
  ; (str "runstate_" *> dsname) --> make_runstate
  ; ( (str "cpu" *> int <&> opt @@ (str "-C" *> int)) --> fun (cpuidx, cstate) ->
      let filename =
        match cstate with None -> "cpu" | Some n -> Printf.sprintf "cpu-C%d" n
      in
      make_ds ~filename ("cpu" ^ string_of_int cpuidx)
    )
  ; (str "cpu_avg" --> fun () -> make_ds ~filename:"cpu_avg" "cpu_avg")
  ; (pcre "pif_" *> dsname) --> make_ds ~filename:"pif"
    (* TODO: could provide info on polarity based on rx/tx and on kind, TICK for errors *)
  ]
  |> route

let classify_dsname dsname =
  let error _ = make_ds dsname in
  dsname |> Tyre.exec classify |> Result.fold ~ok:Fun.id ~error

let classify ~ds_def ~filename ds =
  let open Rrd in
  let override, dsname = classify_dsname ds.ds_name in
  let pathname =
    let name = Fpath.rem_ext filename in
    match override with
    | None ->
        Fpath.(name + "_filtered")
    | Some newname ->
        Fpath.(name + to_string newname)
  in
  (* Logs.debug (fun m -> m "%s -> %a" ds.ds_name Fpath.pp pathname); *)
  let def =
    StringMap.find_opt ds.ds_name ds_def |> Option.value ~default:default_def
  in
  (* can only plot graphs with same units *)
  let extra =
    match def.units with
    | None ->
        (* use RRD type as approximation to "same unit", at least same kind of unit,
           e.g. rate vs duration *)
        Rrd.ds_type_to_string ds.ds_ty
    | Some u ->
        String.take ~sat:Char.Ascii.is_alphanum u
  in
  (Fpath.(pathname + extra |> add_ext "xml"), dsname)

let rrdtool =
  OS.Cmd.resolve (Cmd.v "rrdtool")
  |> Logs.on_error_msg ~use:(fun () -> failwith "rrdtool is not installed")

let rrd_restore filename rrd =
  let filename = Fpath.set_ext "xml" filename in
  Logs.debug (fun m -> m "Writing RRD xml to %a" Fpath.pp filename) ;
  let () =
    Out_channel.with_open_text (Fpath.to_string filename) @@ fun ch ->
    Rrd_unix.to_fd rrd (Unix.descr_of_out_channel ch)
  in
  let dot_rrd = Fpath.set_ext "rrd" filename in
  Logs.debug (fun m -> m "Restoring RRD to %a" Fpath.pp dot_rrd) ;
  Cmd.(rrdtool % "restore" % "-f" % p filename % p dot_rrd)
  |> OS.Cmd.run
  |> Result.map (fun () -> dot_rrd)

let split_rrd ~ds_def ~filename rrd =
  let open Rrd in
  let rrds = Hashtbl.create 3 in
  let original_ds = Hashtbl.create 127 in

  (* split the rrd into multiple rrds based on data source name *)
  let () =
    Logs.info (fun m -> m "classifying data sources") ;
    rrd.rrd_dss
    |> Array.iteri @@ fun i ds ->
       let filename, Ds ds_name = classify ~ds_def ~filename ds in
       let get_i rra = (rra.rra_data.(i), rra.rra_cdps.(i)) in
       let previous =
         Hashtbl.find_opt rrds filename |> Option.value ~default:[]
       in
       Hashtbl.replace original_ds ds_name ds ;
       Hashtbl.replace rrds filename
       @@ (({ds with ds_name}, Array.map get_i rrd.rrd_rras) :: previous)
  in
  Logs.info (fun m -> m "Building and restoring RRDs") ;
  (* now build an RRD and restore it to binary .rrd form *)
  rrds
  |> Hashtbl.iter @@ fun filename lst ->
     Logs.debug (fun m -> m "Building %a" Fpath.pp filename) ;
     let rrd_dss, rrd_rras = List.split lst in
     let rrd_rras =
       rrd.rrd_rras
       |> Array.mapi @@ fun i rra ->
          let rra_seq = List.to_seq rrd_rras in
          let geti a = a.(i) in
          {
            rra with
            rra_data= rra_seq |> Seq.map geti |> Seq.map fst |> Array.of_seq
          ; rra_cdps= rra_seq |> Seq.map geti |> Seq.map snd |> Array.of_seq
          }
     in
     let rrd = {rrd with rrd_dss= Array.of_list rrd_dss; rrd_rras} in
     let data =
       rrd_restore filename rrd
       |> Logs.on_error_msg ~use:(fun () -> failwith "Failed to restore RRD")
     in
     prepare_plots ~filename ~data rrd

type mode = Split | Default | Plot

let parse_ds_def def k v =
  match k with "units" when v <> "unknown" -> {units= Some v} | _ -> def

let parse_ds_defs path =
  Logs.info (fun m -> m "Loading data source definitions from %a" Fpath.pp path) ;
  let fields line =
    line
    |> String.cut ~sep:":"
    |> Option.map @@ fun (k, v) -> (String.trim k, String.trim v)
  in
  let fold (map, key_opt) line =
    match (fields line, key_opt) with
    | Some ("name_label", ds_name), None ->
        (map, Some ds_name) (* start parsing new item *)
    | _, None ->
        (map, None) (* ignore *)
    | None, Some _ ->
        (map, None)
    | Some (k, v), Some ds_name ->
        let map =
          map
          |> Rrd.StringMap.update ds_name @@ fun def ->
             Some (parse_ds_def (Option.value ~default:default_def def) k v)
        in
        (map, Some ds_name)
  in
  OS.File.fold_lines fold (Rrd.StringMap.empty, None) path
  |> Logs.on_error_msg ~use:(fun _ ->
         failwith "Could not parse datasource definitions"
     )
  |> fst

let plot_rrd ~filename rrd =
  let data =
    rrd_restore filename rrd
    |> Logs.on_error_msg ~use:(fun () -> failwith "Failed to restore RRD")
  in
  prepare_plots ~exec:true ~filename ~data rrd

let () =
  let open OS.Arg in
  let level =
    let conv =
      conv ~docv:"LEVEL" Logs.level_of_string Fmt.(option Logs.pp_level)
    in
    opt ~doc:"Set log level" ["log"] conv ~absent:(Some Logs.Debug)
  in
  let mode =
    opt
      ~doc:
        "Used in self-invocation to split rrd into multiple rrds, or to plot \
         an already split rrd"
      ["mode"] ~absent:Default
    @@ enum [("split", Split); ("plot", Plot); ("default", Default)]
  in

  let data_source_list =
    opt ~doc:"Load data source definitions" ~docv:"PATH" ["def"] ~absent:None
      (some path)
  in
  let paths =
    OS.Arg.(
      parse ~doc:"Split and plot xcp-rrdd XML rrd.gz with rrdtool" ~pos:path ()
    )
  in

  Logs.set_level level ;
  let ds_def =
    Option.map parse_ds_defs data_source_list
    |> Option.value ~default:Rrd.StringMap.empty
  in
  match mode with
  | Default ->
      let cmd =
        Cmd.(
          v "find" %% of_values p paths % "-name" % "*.gz" % "-print0"
          |> OS.Cmd.run_out
        )
      in
      (* TODO: forward level *)
      let xargs =
        Cmd.(
          v "xargs"
          % "-0"
          % "-P0"
          % "-n1"
          % Sys.executable_name
          %% of_values ~slip:"--def" p (Option.to_list data_source_list)
          % "--mode=split"
          |> OS.Cmd.run_in
        )
      in
      let res =
        OS.Cmd.out_run_in cmd
        |> Logs.on_error_msg ~use:(fun _ -> exit 1)
        |> xargs
      in
      Logs.on_error_msg ~use:(fun _ -> exit 1) res
  | Split ->
      paths |> List.iter @@ with_input_rrd (split_rrd ~ds_def)
  | Plot ->
      paths |> List.iter @@ with_input_rrd plot_rrd
