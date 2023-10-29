open! Core

module Click_type = struct
  type t =
    | Explore
    | Flag
end

module Click_status = struct
  type t =
    | Clickable
    | Revealed
    | Flagged
  [@@deriving sexp]

  let flag = function
    | Flagged -> Clickable
    | Clickable -> Flagged
    | Revealed -> Revealed
  ;;

  let explore = function
    | (Flagged | Revealed) as t -> t
    | Clickable -> Revealed
  ;;

  let click t = function
    | Click_type.Flag -> flag t
    | Explore -> explore t
  ;;
end

module Cell : sig
  type t

  val is_mine : t -> bool
  val click_status : t -> Click_status.t
  val default_mine : t
  val default_empty : t
  val click : t -> Click_type.t -> t
end = struct
  type t =
    { is_mine : bool
    ; click_status : Click_status.t
    }
  [@@deriving fields]

  let default = Fields.create ~click_status:Click_status.Clickable
  let default_mine = default ~is_mine:true
  let default_empty = default ~is_mine:false

  let click t click_type =
    { t with click_status = Click_status.click (click_status t) click_type }
  ;;
end

module Dimensions = struct
  type t =
    { width : int
    ; height : int
    ; mines : int
    }
end

module Coordinates = struct
  type t =
    { x : int
    ; y : int
    }
end

type t = Cell.t array array

let create { Dimensions.width; height; mines } =
  let buff = Array.make_matrix ~dimx:width ~dimy:height Cell.default_empty in
  let () =
    let random_coords =
      List.cartesian_product (List.init width ~f:Fn.id) (List.init height ~f:Fn.id)
      |> List.sort ~compare:(fun (_ : int * int) (_ : int * int) ->
             Float.ascending (Random.float_range 0. 1.) (Random.float_range 0. 1.))
    in
    List.take random_coords mines
    |> List.iter ~f:(fun (x, y) -> buff.(x).(y) <- Cell.default_mine)
  in
  buff
;;

let click t { Coordinates.x; y } click_type =
  let cell = t.(x).(y) in
  t.(x).(y) <- Cell.click cell click_type
;;

module For_testing = struct
  module Cell = struct
    include Cell

    let mine_char_of_t t =
      match is_mine t with
      | true -> 'b'
      | false -> '.'
    ;;

    let status_char_of_t t =
      match click_status t with
      | Clickable -> 'c'
      | Revealed -> 'r'
      | Flagged -> 'f'
    ;;
  end

  let board_to_string t ~cell_to_char =
    Array.map t ~f:(fun row ->
        Array.map row ~f:cell_to_char |> Array.to_list |> String.of_char_list)
    |> Array.to_list
    |> String.concat ~sep:"\n"
  ;;

  let mine_to_string t = board_to_string t ~cell_to_char:Cell.mine_char_of_t
  let status_to_string t = board_to_string t ~cell_to_char:Cell.status_char_of_t
end
