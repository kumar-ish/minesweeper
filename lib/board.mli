open! Core

type t

module Click_type : sig
  type t =
    | Explore
    | Flag
end

module Coordinates : sig
  type t =
    { x : int
    ; y : int
    }
end

module Dimensions : sig
  type t =
    { width : int
    ; height : int
    ; mines : int
    }
end

val create : Dimensions.t -> t
val click : t -> Coordinates.t -> Click_type.t -> unit
val surrounding_mines : t -> int array array

module For_testing : sig
  val mine_to_string : t -> string
  val status_to_string : t -> string
  val surrounding_mines_to_string : t -> string
end
