open! Core

module Click_type = struct
  type t =
    | Left
    | Right
end

module Click_status = struct
  type t =
    | Clickable
    | Revealed
    | Flagged
  [@@deriving sexp]

  (* let flag = function
    | Flagged -> Clickable
    | Clickable -> Flagged
    | Revealed -> Revealed
  ;;

  let explore = function
    | (Flagged | Revealed) as t -> t
    | Clickable -> Revealed
  ;; *)
end

module Cell : sig
  type t

  val is_mine : t -> bool
  val click_status : t -> Click_status.t
  val default_mine : t
  val default_empty : t
  val click : t -> Click_status.t -> t
end = struct
  type t =
    { is_mine : bool
    ; click_status : Click_status.t
    }
  [@@deriving fields]

  let default = Fields.create ~click_status:Click_status.Clickable
  let default_mine = default ~is_mine:true
  let default_empty = default ~is_mine:false
  let click t new_click_status = { t with click_status = new_click_status }
  (* let click t click_type =
    let click_status =
      match click_type with
      | Click_type.Right  | -> Click_status.flag t.click_status
      | Left -> Click_status.explore t.click_status
    in
    { t with click_status }
  ;; *)
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
  ; dimensions : Dimensions.t
  ; surrounding_mines : int array array
  }

let check_in_bounds { Dimensions.width; height; mines = _ } { Coordinates.x; y } =
  x >= 0 && y >= 0 && x < width && y < height
;;

let matrix_map t ~f = Array.map t ~f:(Array.map ~f)

let mapi_cells cells ~f =
  Array.mapi cells ~f:(fun x -> Array.mapi ~f:(fun y -> f { Coordinates.x; y }))
;;

let neighbouring_coords d { Coordinates.x; y } =
  let cell_breadth = List.range ~stop:`inclusive (-1) 1 in
  let neighbours = List.cartesian_product cell_breadth cell_breadth in
  List.map neighbours ~f:(fun (i, j) -> { Coordinates.x = x + i; y = y + j })
  |> List.filter ~f:(check_in_bounds d)
;;

let surrounding_mines d cells =
  mapi_cells cells ~f:(fun coord (_ : Cell.t) ->
      let neighbours = neighbouring_coords d coord in
      List.sum
        (module Int)
        neighbours
        ~f:(fun { Coordinates.x; y } -> cells.(x).(y) |> Cell.is_mine |> Bool.to_int))
;;

let create ({ Dimensions.width; height; mines } as dimensions) =
  let cells = Array.make_matrix ~dimx:width ~dimy:height Cell.default_empty in
  (* TODO(madhav): Check if this is valid *)
  let () =
    let random_coords =
      List.cartesian_product (List.init width ~f:Fn.id) (List.init height ~f:Fn.id)
      |> List.sort ~compare:(fun (_ : int * int) (_ : int * int) ->
             Float.ascending (Random.float_range 0. 1.) (Random.float_range 0. 1.))
    in
    List.take random_coords mines
    |> List.iter ~f:(fun (x, y) -> cells.(x).(y) <- Cell.default_mine)
  in
  let surrounding_mines = surrounding_mines dimensions cells in
  { cells; dimensions; surrounding_mines }
;;

let rec click t ({ Coordinates.x; y } as coordinates) click_type =
  let previous_cell = t.cells.(x).(y) in
  match click_type, Cell.click_status previous_cell with
  | Click_type.Left, Flagged -> ()
  | Right, Flagged -> t.cells.(x).(y) <- Cell.click previous_cell Clickable
  | Left, Revealed -> chord t coordinates
  | Right, Revealed -> ()
  | Left, Clickable -> explore t coordinates
  | Right, Clickable -> t.cells.(x).(y) <- Cell.click previous_cell Flagged

and chord ({ cells; dimensions; surrounding_mines } as t) ({ Coordinates.x; y } as c) =
  let neighbours = neighbouring_coords dimensions c in
  let non_flagged_neighbours =
    neighbours
    |> List.filter ~f:(fun { Coordinates.x; y } ->
           match Cell.click_status cells.(x).(y) with
           | Flagged -> false
           | _ -> true)
  in
  let num_flagged_mines = List.length neighbours - List.length non_flagged_neighbours in
  if num_flagged_mines = surrounding_mines.(x).(y)
  then List.iter non_flagged_neighbours ~f:(fun c -> click t c Click_type.Right)
  else ()

and explore ({ cells; dimensions; surrounding_mines } as t) ({ Coordinates.x; y } as c) =
  let current_cell = cells.(x).(y) in
  if surrounding_mines.(x).(y) = 0
  then
    neighbouring_coords dimensions c |> List.iter ~f:(fun c -> click t c Click_type.Right)
  else if Cell.is_mine current_cell
  then ()
  else t.cells.(x).(y) <- Cell.click current_cell Revealed
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

  let mine_to_string { cells; dimensions = _; surrounding_mines = _ } =
    board_to_string cells ~cell_to_string:Cell.mine_char_of_t
  ;;

  let status_to_string { cells; dimensions = _; surrounding_mines = _ } =
    board_to_string cells ~cell_to_string:Cell.status_char_of_t
  ;;

  let surrounding_mines_to_string t =
    board_to_string t.surrounding_mines ~cell_to_string:(fun c -> Int.to_string c)
  ;;
end
