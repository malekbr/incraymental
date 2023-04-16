open! Core

type -'row t =
  { width : int
  ; height : int
  ; sprite_sheet : Raylib.Texture2D.t
  ; framecounts : int array
  ; row_selector : 'row -> int
  ; row_equal : 'row -> 'row -> bool
  }

module State = struct
  type 'row t =
    { selected_row : 'row
    ; current_frame : [ `Cycle of Time_ns.Span.t | `Constant of int ]
    }
  [@@deriving equal]
end

module Int_pair = Comparable.Make_plain (struct
  type t = int * int [@@deriving sexp_of, compare]
end)

let load_exn sprite_sheet ~framecounts ~width ~height ~row_selector ~row_equal =
  let sheet_width = Raylib.Image.width sprite_sheet in
  let sheet_height = Raylib.Image.height sprite_sheet in
  if width <= 0
     || height <= 0
     || sheet_width mod width <> 0
     || sheet_height mod height <> 0
  then
    raise_s
      [%message
        "Invalid width/height. Needs to be an integral subdivision of the sheet."
          (width : int)
          (height : int)
          (sheet_width : int)
          (sheet_height : int)];
  let framecounts =
    match framecounts with
    | `Constant list ->
      if List.length list > sheet_height / height
      then raise_s [%message "Too many entries in the framecount"];
      Array.of_list_map list ~f:(fun count ->
        if count < 0 then raise_s [%message "Count cannot be negative"];
        if count > sheet_width / width then raise_s [%message "Too many frames for a row"];
        count)
    | `Auto_detect ->
      let filled_sheet_positions =
        Sequence.fold
          (Sequence.range 0 sheet_width)
          ~init:Int_pair.Set.empty
          ~f:(fun init x ->
          Sequence.fold (Sequence.range 0 sheet_height) ~init ~f:(fun init y ->
            if Raylib.get_image_color sprite_sheet x y |> Raylib.Color.a |> ( < ) 0
            then Set.add init (x / width, y / height)
            else init))
      in
      Array.init (sheet_height / height) ~f:(fun y ->
        Sequence.range
          ~stride:(-1)
          ~start:`exclusive
          ~stop:`inclusive
          (sheet_width / width)
          0
        |> Sequence.find ~f:(fun x -> Set.mem filled_sheet_positions (x, y))
        |> Option.value_map ~default:0 ~f:Int.succ)
  in
  { width
  ; height
  ; sprite_sheet = Raylib.load_texture_from_image sprite_sheet
  ; framecounts
  ; row_selector
  ; row_equal
  }
;;

let create
  t
  ?(flip_x = Incr.return false)
  ?(flip_y = Incr.return false)
  ?(tint = Incr.return Raylib.Color.white)
  state
  ~target
  ~clock
  =
  let input = Incr.map state ~f:Fn.id in
  Incr.set_cutoff input (Incr.Cutoff.of_equal (State.equal t.row_equal));
  let of_state { State.selected_row; current_frame } =
    let row = t.row_selector selected_row in
    if t.framecounts.(row) <= 0
    then raise_s [%message "Invalid row" (row : int) (t.framecounts : int array)];
    let%map.Incr frame =
      match current_frame with
      | `Constant frame ->
        if frame >= t.framecounts.(row)
        then raise_s [%message "Frame out of bounds" (row : int) (frame : int)];
        Incr.return frame
      | `Cycle step ->
        let variable = Incr.Var.create 0 in
        let%bind.Incr () = Incr.Clock.at_intervals clock step in
        Incr.Var.replace variable ~f:(fun i -> Int.succ i mod t.framecounts.(row));
        Incr.Var.watch variable
    in
    row, frame
  in
  let target =
    match%map.Incr target with
    | `Position { Draw_actions.Vector2.x; y } ->
      { Draw_actions.Rectangle.x; y; w = Float.of_int t.width; h = Float.of_int t.height }
    | `Rectangle rect -> rect
  in
  let%bind.Incr state = input in
  let%map.Incr row, frame = of_state state
  and tint = tint
  and flip_x = flip_x
  and flip_y = flip_y
  and target = target in
  Draw_actions.Instructions.Texture
    { texture = t.sprite_sheet
    ; source =
        { x = Float.of_int (frame * t.width)
        ; y = Float.of_int (row * t.height)
        ; w = Float.of_int (if flip_x then -t.width else t.width)
        ; h = Float.of_int (if flip_y then -t.height else t.height)
        }
    ; target
    ; tint
    }
;;
