module type RANGE = sig
  type bound

  type t = {from : bound; upto : bound}

  val encoding : t Data_encoding.t

  val pp : Format.formatter -> t -> unit

  val is_empty : t -> bool

  val length : t -> int

  val split : reverse:bool -> max_range_size:int -> t -> t Seq.t
end

module Level_range : RANGE with type bound = Int32.t

module Introspection : sig
  type step =
    | Fetching_headers
    | Write_headers
    | Waiting_for_fetching_operations
    | Fetching_operations
    | Waiting_for_validation
    | Validating
    | Processed

  type interval = {start : Time.System.t; span : Ptime.Span.t}

  module Table : Hashtbl.S with type key = Level_range.t

  module Set : Set.S with type elt = Level_range.t

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

  type info = {
    started : Time.System.t;
    mutable status : (Time.System.t * unit tzresult) Lwt.state;
    mutable ranges_processed : Set.t;
    range_info : range_info Table.t;
    target : Block_header.t;
    mutable validated_blocks : Int32.t;
    mutable blocks_to_validate : Int32.t;
  }

  type state =
    | Active of info  (** Info of the current job *)
    | Inactive of info option
        (** Contains info of the current task if the bootstrapper is [Active]
   or the [info] of the previous task if there is one. *)

  val pp : Format.formatter -> info -> unit

  val encoding : state Data_encoding.t
end
