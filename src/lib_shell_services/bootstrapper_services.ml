module type RANGE = sig
  type bound

  type t = {from : bound; upto : bound}

  val encoding : t Data_encoding.t

  val pp : Format.formatter -> t -> unit

  val is_empty : t -> bool

  val length : t -> int

  val split : reverse:bool -> max_range_size:int -> t -> t Seq.t
end

let rec unfold f u () =
  match f u with None -> Seq.Nil | Some (x, u') -> Cons (x, unfold f u')

module Level_range : RANGE with type bound = Int32.t = struct
  type bound = Int32.t

  type t = {from : Int32.t; upto : Int32.t}

  let encoding =
    let open Data_encoding in
    conv
      (fun {from; upto} -> (from, upto))
      (fun (from, upto) -> {from; upto})
      (obj2 (req "from" Data_encoding.int32) (req "upto" Data_encoding.int32))

  let pp fmt {from; upto} = Format.fprintf fmt "%ld-%ld" from upto

  let is_empty {from; upto} = Int32.sub upto from = 0l

  let length {from; upto} = Int32.to_int (Int32.sub upto from)

  type range_splitter = {range : t; started : bool; over : bool}

  let next_range ~reverse ~max_range_size first_range last_range
      ({range; started; over} as state) =
    if not started then Some (first_range, {state with started = true})
    else if range.upto <= range.from then
      if over || is_empty last_range then None
      else Some (last_range, {state with over = true})
    else
      let (from, upto, next_range) =
        if reverse then (
          let new_upto = Int32.sub range.upto max_range_size in
          assert (new_upto >= range.from) ;
          (range.from, new_upto, {from = new_upto; upto = range.upto}) )
        else
          let new_from = Int32.add range.from max_range_size in
          assert (new_from <= range.upto) ;
          (new_from, range.upto, {from = range.from; upto = new_from})
      in
      Some (next_range, {range = {from; upto}; started; over})

  let split ~reverse ~max_range_size ({from; upto} as range) =
    if max_range_size <= 0 then
      invalid_arg "Ranger.Int32.split: max_range_size <= 0" ;
    let floor_size range_size n =
      (* n - (n mod size)) *)
      Int32.sub n (Int32.rem n range_size)
    in
    let ceil_size range_size n =
      (* n + (size - (n mod size)) *)
      Int32.add n (Int32.sub range_size (Int32.rem n range_size))
    in
    if upto <= from then Seq.empty
    else if Int32.sub upto from < Int32.of_int max_range_size then
      Seq.return range
    else
      let max_range_size = Int32.of_int max_range_size in
      let from_rounded = ceil_size max_range_size from in
      let upto_rounded = floor_size max_range_size upto in
      let bottom_range = {from; upto = from_rounded} in
      let top_range = {from = upto_rounded; upto} in
      assert (
        from <= from_rounded
        && from_rounded <= upto_rounded
        && upto_rounded <= upto ) ;
      let (last_range, first_range) =
        if reverse then (bottom_range, top_range) else (top_range, bottom_range)
      in
      unfold
        (next_range ~reverse ~max_range_size first_range last_range)
        {
          range = {from = from_rounded; upto = upto_rounded};
          started = is_empty first_range;
          over = false;
        }
end

module Introspection = struct
  type step =
    | Fetching_headers
    | Write_headers
    | Waiting_for_fetching_operations
    | Fetching_operations
    | Waiting_for_validation
    | Validating
    | Processed

  type interval = {start : Time.System.t; span : Ptime.Span.t}

  type range_info = {
    mutable current_step : step;
    mutable beginning : Time.System.t;
    mutable fetching_headers_time : Ptime.Span.t;
    mutable write_headers_time : Ptime.Span.t;
    mutable waiting_for_fetching_operations_time : Ptime.Span.t;
    mutable fetching_operations_time : Ptime.Span.t;
    mutable waiting_for_validation_time : Ptime.Span.t;
    mutable validating_time : Ptime.Span.t;
    mutable retries : int;
  }

  module Table = Hashtbl.Make (struct
    type t = Level_range.t

    let equal = ( = )

    let hash = Hashtbl.hash
  end)

  module Set = Set.Make (struct
    type t = Level_range.t

    let compare = compare
  end)

  type info = {
    started : Time.System.t;
    mutable status : (Time.System.t * unit tzresult) Lwt.state;
    mutable ranges_processed : Set.t;
    range_info : range_info Table.t;
    target : Block_header.t;
    mutable validated_blocks : Int32.t;
    mutable blocks_to_validate : Int32.t;
  }

  type state = Active of info | Inactive of info option

  let compare_bindings (_, {beginning; _}) (_, {beginning = beginning'; _}) =
    Ptime.compare beginning beginning'

  let pp_step fmt = function
    | Fetching_headers ->
        Format.fprintf fmt "fetching headers"
    | Write_headers ->
        Format.fprintf fmt "write headers"
    | Waiting_for_fetching_operations ->
        Format.fprintf fmt "waiting for fetching operations"
    | Fetching_operations ->
        Format.fprintf fmt "fetching operations"
    | Waiting_for_validation ->
        Format.fprintf fmt "waiting for validation"
    | Validating ->
        Format.fprintf fmt "validating"
    | Processed ->
        Format.fprintf fmt "Processed"

  let pp_status fmt = function
    | Lwt.Return (over, Ok ()) ->
        Format.fprintf
          fmt
          "Successfully finished at %a@."
          Time.System.pp_hum
          over
    | Lwt.Return (over, Error _) ->
        Format.fprintf fmt "Failed at %a@." Time.System.pp_hum over
    | Lwt.Fail exn ->
        Format.fprintf fmt "Promise failed with %s@." (Printexc.to_string exn)
    | Lwt.Sleep ->
        Format.fprintf fmt "Ongoing@."

  let pp_range_info fmt
      { current_step;
        beginning;
        fetching_headers_time;
        write_headers_time;
        waiting_for_fetching_operations_time;
        fetching_operations_time;
        waiting_for_validation_time;
        validating_time;
        retries } =
    Format.fprintf fmt "Started: %a@." Time.System.pp_hum beginning ;
    Format.fprintf fmt "Current step: %a@." pp_step current_step ;
    Format.fprintf fmt "Retries: %d@." retries ;
    List.iter
      (fun (name, span) ->
        if Ptime.Span.(span <> zero) then
          Format.fprintf fmt "%s: %a@." name Ptime.Span.pp span)
      [ ("fetching headers", fetching_headers_time);
        ("write headers", write_headers_time);
        ( "waiting for fetching operations",
          waiting_for_fetching_operations_time );
        ("fetching operations", fetching_operations_time);
        ("waiting for validation", waiting_for_validation_time);
        ("validating", validating_time) ]

  let pp fmt
      { started;
        status;
        ranges_processed;
        range_info;
        target;
        validated_blocks;
        blocks_to_validate } =
    Format.fprintf fmt "BOOTSTRAPPER INFO:@." ;
    Format.fprintf
      fmt
      "Target hash     : %a@."
      Block_hash.pp
      (Block_header.hash target) ;
    Format.fprintf
      fmt
      "Target level    : %ld@."
      target.Block_header.shell.level ;
    Format.fprintf fmt "Starting time    : %a@." Time.System.pp_hum started ;
    Format.fprintf fmt "Status           : %a@." pp_status status ;
    Format.fprintf
      fmt
      "Range processed  : %d@."
      (Set.cardinal ranges_processed) ;
    Format.fprintf fmt "Validated blocks : %ld@." validated_blocks ;
    Format.fprintf fmt "blocks to validate : %ld@." blocks_to_validate ;
    Format.fprintf
      fmt
      "Range processed  : %d@."
      (Set.cardinal ranges_processed) ;
    Format.fprintf fmt "Range statistics:@." ;
    let ranges_list : (Level_range.t * range_info) list ref = ref [] in
    Table.iter
      (fun range range_info ->
        ranges_list := (range, range_info) :: !ranges_list)
      range_info ;
    let sorted_ranges = List.fast_sort compare_bindings !ranges_list in
    List.iter
      (fun (range, range_info) ->
        Format.fprintf
          fmt
          "Range %a:@.%a@."
          Level_range.pp
          range
          pp_range_info
          range_info)
      sorted_ranges
end
