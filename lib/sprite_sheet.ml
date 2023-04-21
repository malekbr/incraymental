open! Core
open! Bonsai
open Bonsai.Let_syntax

type -'row t =
  { width : int
  ; height : int
  ; sprite_sheet : Draw_actions.Loadable_texture.t
  ; framecounts : int array
  ; row_selector : 'row -> int
  ; row_equal : 'row -> 'row -> bool
  }

module State = struct
  type 'row t =
    { selected_row : 'row
    ; current_frame : [ `Cycle_every_n_frames of int | `Constant of int ]
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
  ; sprite_sheet = Draw_actions.Loadable_texture.create sprite_sheet
  ; framecounts
  ; row_selector
  ; row_equal
  }
;;

let cycle_frames ~every_n_frame ~count =
  let%sub frame, set_frame = state (module Int) ~default_model:0 in
  let%sub remaining_frames, set_remaining_frames = state (module Int) ~default_model:1 in
  let%sub () =
    Edge.lifecycle
      ~on_activate:
        (let%map set_remaining_frames = set_remaining_frames
         and every_n_frame = every_n_frame in
         set_remaining_frames every_n_frame)
      ~after_display:
        (let%map set_remaining_frames = set_remaining_frames
         and remaining_frames = remaining_frames
         and count = count
         and every_n_frame = every_n_frame
         and frame = frame
         and set_frame = set_frame in
         let remaining_frames = remaining_frames - 1 in
         if remaining_frames < 0
         then assert false
         else if remaining_frames = 0
         then
           Ui_effect.Many
             [ set_frame ((frame + 1) mod count); set_remaining_frames every_n_frame ]
         else set_remaining_frames remaining_frames)
      ()
  in
  return frame
;;

let create
  t
  ?(flip_x = Bonsai.Value.return false)
  ?(flip_y = Bonsai.Value.return false)
  ?(tint = Bonsai.Value.return Raylib.Color.white)
  state
  ~target
  =
  let state = Bonsai.Value.cutoff state ~equal:(State.equal t.row_equal) in
  let%sub { State.selected_row; current_frame } = return state in
  let row =
    let%map selected_row = selected_row in
    let row = t.row_selector selected_row in
    if t.framecounts.(row) <= 0
    then raise_s [%message "Invalid row" (row : int) (t.framecounts : int array)];
    row
  in
  let%sub frame =
    match%sub current_frame with
    | `Constant frame ->
      let%arr frame = frame
      and row = row in
      if frame >= t.framecounts.(row)
      then raise_s [%message "Frame out of bounds" (row : int) (frame : int)];
      frame
    | `Cycle_every_n_frames every_n_frame ->
      cycle_frames
        ~every_n_frame
        ~count:
          (let%map row = row in
           t.framecounts.(row))
  in
  let target =
    match%map target with
    | `Position { Draw_actions.Vector2.x; y } ->
      { Draw_actions.Rectangle.x; y; w = Float.of_int t.width; h = Float.of_int t.height }
    | `Rectangle rect -> rect
  in
  let%arr row = row
  and frame = frame
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
