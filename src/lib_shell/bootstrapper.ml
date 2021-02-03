(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

type block = Block_header.t * Operation.t list list

type consistency_checker = Block_hash.t -> bool Lwt.t

type getters = {
  get_current_head : unit -> Block_header.t Lwt.t;
  get_headers :
    target:Block_header.t ->
    from:Int32.t ->
    upto:Int32.t ->
    (consistency_checker * Block_header.t list) option Lwt.t;
  get_operations : Block_header.t list -> block list option Lwt.t;
  when_unable_to_fetch : unit -> unit tzresult Lwt.t;
  validate : block -> unit tzresult Lwt.t;
}

type configuration = {
  root_path : string;
  temporary_path : string;
  range_size : int;
  parallel_ranges_header_fetched : int;
  parallel_ranges_operations_fetched : int;
  getters : getters;
}

(* FIXME: Replace this by Lwt_io.with_file maybe? *)
let with_open_file ~flags ?(perms = 0o640) filename task =
  Lwt.catch
    (fun () ->
      Lwt_unix.openfile filename flags perms >>= fun x -> Lwt.return (Ok x))
    (function
      | Unix.Unix_error (error, f, file) ->
          Lwt.return (Error (error, f, file))
      | exn ->
          raise exn)
  (* Lwt.fail? *)
  >>= function
  | Error _ as x ->
      Lwt.return x
  | Ok fd ->
      let close () =
        Lwt.catch
          (fun () -> Lwt_unix.close fd)
          (function
            | Unix.Unix_error _ ->
                (* already closed *) Lwt.return_unit
            | exn ->
                raise exn)
        (* Lwt.fail? *)
      in
      Lwt.catch
        (fun () -> task fd)
        (fun exn -> close () >>= fun () -> raise exn)
      >>= fun task_result -> close () >>= fun () -> Lwt.return (Ok task_result)

let with_open_out file task =
  with_open_file ~flags:[O_WRONLY; O_CREAT; O_TRUNC; O_CLOEXEC] file task

let with_open_in file task =
  with_open_file ~flags:[O_RDONLY; O_CLOEXEC] file task

(* This is to avoid file corruption *)
let with_atomic_open_out configuration filename f =
  Filename.set_temp_dir_name configuration.temporary_path ;
  let temp_file = Filename.temp_file (Filename.basename filename) ".tmp" in
  with_open_out temp_file f
  >>=? fun res ->
  Lwt_unix.rename temp_file filename >>= fun () -> Lwt.return (Ok res)

(* FIXME: This type will be implemented in a next commit *)
type info = unit

type state = Active of info | Inactive of info option

type t = {
  mutable job : unit tzresult Lwt.t option;
  mutable last_result : error trace option;
  mutable state : state;
  mutable next_target : Block_header.t option;
  configuration : configuration;
}

module Level_range = Ranger.Int32
module Range_worker = Ranger.Make (Level_range)

let range_filename configuration range =
  let basename = Format.asprintf "%a" Level_range.pp range in
  Filename.concat configuration.root_path basename

let range_header_encoding range_size : Block_header.t list Data_encoding.t =
  Data_encoding.(
    list
      ~max_length:range_size
      (Data_encoding.dynamic_size ~kind:`Uint30 Block_header.encoding))

module Headers : sig
  (** [parameters target range] initializes the [parameters] to give
     to the [ranger] worker to fetch headers and store them onto the
     disk. The [range] is the initial range. The worker may return a
     list of parallel errors. *)
  val parameters :
    configuration ->
    target:Block_header.t ->
    Level_range.t ->
    error trace Range_worker.parameters
end = struct
  type error += Fetching_headers_error of Level_range.t * string

  let () =
    register_error_kind
      `Permanent
      ~id:"fetching_headers_error"
      ~title:"Fetching headers error"
      ~description:"Error while fetching headers"
      ~pp:(fun ppf (range, s) ->
        Format.fprintf
          ppf
          "Error while fetching headers for range '%a': %s@."
          Level_range.pp
          range
          s)
      Data_encoding.(
        obj2 (req "range" Level_range.encoding) (req "explanation" string))
      (function
        | Fetching_headers_error (range, s) -> Some (range, s) | _ -> None)
      (fun (range, s) -> Fetching_headers_error (range, s))

  let fail range s = Error_monad.fail (Fetching_headers_error (range, s))

  let parallel_task configuration target (range : Level_range.t) =
    configuration.getters.get_headers ~target ~from:range.from ~upto:range.upto
    >>= function
    | None ->
        return_none
    | Some (_, []) ->
        fail
          range
          "Unexpected error. The function 'parallel_task' does not fulfilled \
           its specification. Please, report this problem."
    | Some (consistency_checker, first_header :: headers) ->
        (* The [first_header] is used by the sequential tasks of the
          next range for its consistency check. *)
        let fetched_headers_length = List.length (first_header :: headers) in
        let expected_length = Level_range.length range in
        if fetched_headers_length <> expected_length then
          fail
            range
            "Unexpected error. The function 'get_headers' does not fulfilled \
             its specification. Please, report this problem."
        else return_some (consistency_checker, first_header, headers)

  type write_range_error =
    | Unix_error of Unix.error * string * string
    | Encoding_error of Data_encoding.Binary.write_error

  let pp_write_range_error fmt = function
    | Unix_error (unix_error, f, string) ->
        Format.fprintf
          fmt
          "Unix error: (%s, %s, %s)"
          (Unix.error_message unix_error)
          f
          string
    | Encoding_error write_error ->
        Format.fprintf
          fmt
          "%a@."
          Data_encoding.Binary.pp_write_error
          write_error

  let write_range configuration range headers :
      (unit, write_range_error) result Lwt.t =
    let task fd =
      let bytes =
        Data_encoding.Binary.to_bytes
          (range_header_encoding configuration.range_size)
          headers
      in
      match bytes with
      | Ok bytes ->
          Lwt_utils_unix.write_bytes fd bytes >>= fun () -> return_unit
      | Error err ->
          Lwt.return (Error (Encoding_error err))
    in
    let filename = range_filename configuration range in
    with_atomic_open_out configuration filename task
    >>= function
    | Error (err, f, file) ->
        Lwt.return (Error (Unix_error (err, f, file)))
    | Ok (Error err) ->
        Lwt.return (Error err)
    | Ok (Ok ()) ->
        Lwt.return (Ok ())

  let sequential_task configuration range trigger
      (consistency_checker, first_header, headers) =
    consistency_checker trigger
    >>= fun consistent ->
    (* It is important not to fail here because we may receive
       inconsistent data. If that so the behavior of the [ranger] is
       to run again the [parallel_task] and then the
       [sequential_task]. *)
    if not consistent then return_none
    else
      write_range configuration range (first_header :: headers)
      >>= function
      | Ok () ->
          return_some first_header.shell.predecessor
      | Error err ->
          fail
            range
            (Format.asprintf
               "Unable to write range: %a"
               pp_write_range_error
               err)

  let parameters configuration ~target range =
    let tasks_in_parallel = configuration.parallel_ranges_header_fetched in
    let starts_with = Lwt.return (Block_header.hash target) in
    let max_range_size = configuration.range_size in
    (* The range will be split in smaller ranges. These ranges will be
       processed from top to bottom. *)
    let reverse = true in
    let parallel_task = parallel_task configuration target in
    let sequential_task = sequential_task configuration in
    let recovery_task _ = configuration.getters.when_unable_to_fetch () in
    (* FIXME: Implement recovery mode in a further commit *)
    let filter _ = Lwt.return_none in
    Range_worker.E
      {
        tasks_in_parallel;
        starts_with;
        range;
        max_range_size;
        reverse;
        parallel_task;
        sequential_task;
        recovery_task;
        filter;
      }
end

module Operations_and_validation : sig
  (** [parameters target range] initializes the [parameters] to give
     to the [ranger] worker fetching operations and validating
     blocks. The [range] is the initial range. The worker may return a
     list of parallel errors. *)
  val parameters :
    configuration ->
    when_to_start:unit Lwt.t ->
    Level_range.t ->
    error trace Range_worker.parameters
end = struct
  type error +=
    | Fetching_operations_validation_error of
        Level_range.t * Block_header.t option * string

  let () =
    register_error_kind
      `Permanent
      ~id:"fetching_operations_validation_error"
      ~title:"Fetching operations or validation error"
      ~description:
        "An error occured during fetching of operations or the validation of \
         a block."
      ~pp:(fun ppf (range, header, explanation) ->
        Format.fprintf
          ppf
          "An error occured for range %a (for block %a): %s.@."
          Level_range.pp
          range
          (Format.pp_print_option
             ~none:(fun fmt () -> Format.fprintf fmt "none")
             Block_header.pp)
          header
          explanation)
      Data_encoding.(
        obj3
          (req "range" Level_range.encoding)
          (req "header" (dynamic_size (option Block_header.encoding)))
          (req "explanation" string))
      (function
        | Fetching_operations_validation_error (range, header, explanation) ->
            Some (range, header, explanation)
        | _ ->
            None)
      (fun (range, header, explanation) ->
        Fetching_operations_validation_error (range, header, explanation))

  let trace range header s r =
    Error_monad.trace
      (Fetching_operations_validation_error (range, header, s))
      r

  let fail range header s =
    Error_monad.fail (Fetching_operations_validation_error (range, header, s))

  let parallel_task configuration range : block list option tzresult Lwt.t =
    let range_filename = range_filename configuration range in
    with_open_in range_filename (fun fd ->
        let size = (Unix.fstat (Lwt_unix.unix_file_descr fd)).Unix.st_size in
        let buffer = Bytes.create size in
        Lwt_utils_unix.read_bytes fd buffer
        >>= fun () ->
        Lwt.return
          (Data_encoding.Binary.of_bytes
             (range_header_encoding configuration.range_size)
             buffer))
    >>= function
    | Ok (Ok headers) -> (
        configuration.getters.get_operations headers
        >>= function None -> return_none | Some blocks -> return_some blocks )
    | Ok (Error error) ->
        fail
          range
          None
          (Format.asprintf
             "Error while decoding file: %a"
             Data_encoding.Binary.pp_read_error
             error)
    | Error (unix_error, f, string) ->
        fail
          range
          None
          (Format.asprintf
             "Error while reading header file: (%s, %s, %s)"
             (Unix.error_message unix_error)
             f
             string)

  let sequential_task configuration range () blocks =
    List.iter_es
      (fun block ->
        trace
          range
          (Some (fst block))
          "Validation failed"
          (configuration.getters.validate block))
      blocks
    >>=? fun () -> return_some ()

  let parameters configuration ~when_to_start range =
    let tasks_in_parallel = configuration.parallel_ranges_operations_fetched in
    let starts_with = when_to_start in
    let max_range_size = configuration.range_size in
    (* ranges are processed bottom to top *)
    let reverse = false in
    let parallel_task = parallel_task configuration in
    let sequential_task = sequential_task configuration in
    let recovery_task _ = configuration.getters.when_unable_to_fetch () in
    let filter _ = Lwt.return_none in
    Range_worker.E
      {
        tasks_in_parallel;
        starts_with;
        range;
        max_range_size;
        reverse;
        parallel_task;
        sequential_task;
        recovery_task;
        filter;
      }
end

(* Given a target compute the range to bootstrap from. *)
let prepare_worker :
    configuration ->
    target:Block_header.t ->
    (Block_header.t * Level_range.t) tzresult Lwt.t =
 fun configuration ~target ->
  configuration.getters.get_current_head ()
  >>= fun start ->
  (* When target <= start, the worker will do nothing. *)
  return
    (target, Level_range.{from = start.shell.level; upto = target.shell.level})

type error += Bootstrapper_error

let () =
  register_error_kind
    `Permanent
    ~id:"bootstrapper_error"
    ~title:"Bootstrapper error"
    ~description:"The bootstrapper failed."
    ~pp:(fun ppf () -> Format.fprintf ppf "Bootstrapper failed.@.")
    Data_encoding.unit
    (function Bootstrapper_error -> Some () | _ -> None)
    (fun () -> Bootstrapper_error)

let main_worker configuration target range =
  if Level_range.length range <= 0 then return_unit
  else
    let fetch_headers_parameters =
      Headers.parameters configuration ~target range
    in
    let headers_worker = Range_worker.create fetch_headers_parameters in
    Range_worker.wait headers_worker
    >>=? fun () ->
    let operations_and_validation_parameters =
      Operations_and_validation.parameters
        configuration
        ~when_to_start:Lwt.return_unit
        range
    in
    (* We start fetching operations and validating blocks once all the
       headers have been fetched and stored onto the disk. *)
    let operations_and_validation_parameters_worker =
      Range_worker.create operations_and_validation_parameters
    in
    Range_worker.wait operations_and_validation_parameters_worker

type error += Bootstrapper_configuration_error of string

let () =
  register_error_kind
    `Permanent
    ~id:"bootstrapper_configuration_error"
    ~title:"Bootstrapper configuration error"
    ~description:"Wrong configuration for bootstrapper"
    ~pp:(fun ppf s -> Format.fprintf ppf "Bad configuration: %s@." s)
    Data_encoding.(obj1 (req "explanation" string))
    (function Bootstrapper_configuration_error s -> Some s | _ -> None)
    (fun s -> Bootstrapper_configuration_error s)

(* Check that a given path already exists and is a directory,
   otherwise tries to create it. *)
let check_path_is_dir_or_make_it path =
  ( if not (Sys.file_exists path) then
    try Unix.mkdir path 0o755 ; return_unit
    with exn ->
      Error_monad.fail
        (Bootstrapper_configuration_error
           (Format.asprintf
              "Unable to create directory: %s (%s)@."
              path
              (Printexc.to_string exn)))
  else return_unit )
  >>=? fun () ->
  if not (Sys.is_directory path) then
    Error_monad.fail
      (Bootstrapper_configuration_error
         (Format.asprintf "Path '%s' exists and is not a directory@." path))
  else return_unit

(* Check that the configuration given to the bootstrapper is
   consistent. *)
let check_configuration configuration =
  if configuration.range_size <= 0 then
    Error_monad.fail
      (Bootstrapper_configuration_error
         (Format.asprintf
            "Range_size (%d) should be strictly positive."
            configuration.range_size))
  else if configuration.parallel_ranges_header_fetched <= 0 then
    Error_monad.fail
      (Bootstrapper_configuration_error
         (Format.asprintf
            "Parallel ranges header fetched (%d) should be strictly positive."
            configuration.range_size))
  else if configuration.parallel_ranges_operations_fetched <= 0 then
    Error_monad.fail
      (Bootstrapper_configuration_error
         (Format.asprintf
            "Parallel ranges operations fetched (%d) should be strictly \
             positive."
            configuration.range_size))
  else
    check_path_is_dir_or_make_it configuration.root_path
    >>=? fun () -> check_path_is_dir_or_make_it configuration.temporary_path

let create configuration =
  check_configuration configuration
  >>= function
  | Error err ->
      Error_monad.trace Bootstrapper_error (Lwt.return (Error err))
  | Ok () ->
      return
        {
          job = None;
          state = Inactive None;
          last_result = None;
          configuration;
          next_target = None;
        }

let main_job t target =
  prepare_worker t.configuration ~target
  >>=? fun (target, range) -> main_worker t.configuration target range

let notify_target t ~target =
  match t.job with
  | None ->
      let rec job target =
        t.state <- Active () ;
        let p = main_job t target in
        Lwt.on_termination p (fun _ -> t.state <- Inactive (Some ())) ;
        p
        >>=? fun () ->
        match t.next_target with
        | None ->
            t.job <- None ;
            t.state <- Inactive (Some ()) ;
            return_unit
        | Some target ->
            t.next_target <- None ;
            job target
      in
      t.job <- Some (job target)
  | Some _ ->
      t.next_target <- Some target

let state t = t.state

let wait t =
  match t.job with None -> return_unit | Some job -> Lwt.protected job

let cancel t = match t.job with None -> () | Some job -> Lwt.cancel job
