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

type t =
  { cells : Cell.t array array
  ; width : int
  ; height : int
  }

let check_in_bounds { cells = _; width; height } { Coordinates.x; y } =
  x >= 0 && y >= 0 && x < width && y < height
;;

let create { Dimensions.width; height; mines } =
  let cells = Array.make_matrix ~dimx:width ~dimy:height Cell.default_empty in
  let () =
    let random_coords =
      List.cartesian_product (List.init width ~f:Fn.id) (List.init height ~f:Fn.id)
      |> List.sort ~compare:(fun (_ : int * int) (_ : int * int) ->
             Float.ascending (Random.float_range 0. 1.) (Random.float_range 0. 1.))
    in
    List.take random_coords mines
    |> List.iter ~f:(fun (x, y) -> cells.(x).(y) <- Cell.default_mine)
  in
  { cells; width; height }
;;

let matrix_map t ~f = Array.map t ~f:(Array.map ~f)

let mapi_cells t ~f =
  Array.mapi t.cells ~f:(fun x -> Array.mapi ~f:(fun y -> f { Coordinates.x; y }))
;;

(* let mapi_cells t ~f = Array.map t.cells ~f *)

let neighbouring_coords t { Coordinates.x; y } =
  let cell_breadth = List.range ~stop:`inclusive (-1) 1 in
  let neighbours = List.cartesian_product cell_breadth cell_breadth in
  List.map neighbours ~f:(fun (i, j) -> { Coordinates.x = x + i; y = y + j })
  |> List.filter ~f:(check_in_bounds t)
;;

let surrounding_mines t =
  mapi_cells t ~f:(fun coords (_ : Cell.t) ->
      let neighbours = neighbouring_coords t coords in
      List.sum
        (module Int)
        neighbours
        ~f:(fun { Coordinates.x; y } -> t.cells.(x).(y) |> Cell.is_mine |> Bool.to_int))
;;

let _expand _cells _x _y = ()

let click { cells; width = _; height = _ } { Coordinates.x; y } click_type =
  let cell = cells.(x).(y) in
  let resulting_cell = Cell.click cell click_type in
  cells.(x).(y) <- resulting_cell;
  match Cell.click_status resulting_cell with
  | Clickable | Flagged -> ()
  | Revealed -> _expand cells x y
;;

module For_testing = struct
  module Cell = struct
    include Cell

    let mine_char_of_t t =
      match is_mine t with
      | true -> "b"
      | false -> "."
    ;;

    let status_char_of_t t =
      match click_status t with
      | Clickable -> "c"
      | Revealed -> "r"
      | Flagged -> "f"
    ;;
  end

  let board_to_string cells ~cell_to_string =
    matrix_map cells ~f:cell_to_string
    |> Array.map ~f:(fun row -> Array.to_list row |> String.concat)
    |> Array.to_list
    |> String.concat ~sep:"\n"
  ;;

  let mine_to_string { cells; width = _; height = _ } =
    board_to_string cells ~cell_to_string:Cell.mine_char_of_t
  ;;

  let status_to_string { cells; width = _; height = _ } =
    board_to_string cells ~cell_to_string:Cell.status_char_of_t
  ;;

  let surrounding_mines_to_string t =
    board_to_string (surrounding_mines t) ~cell_to_string:(fun c -> Int.to_string c)
  ;;
end
