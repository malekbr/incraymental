open! Core
open! Bonsai
open Bonsai.Let_syntax

let width = 640
let height = 480

module Direction = struct
  type t =
    | Down
    | Left
    | Right
    | Up
  [@@deriving variants, sexp, equal]

  let unit_vector = function
    | Down -> { Draw_actions.Vector2.x = 0.; y = 1. }
    | Up -> { Draw_actions.Vector2.x = 0.; y = -1. }
    | Left -> { Draw_actions.Vector2.x = -1.; y = 0. }
    | Right -> { Draw_actions.Vector2.x = 1.; y = 0. }
  ;;
end

module Sprite_files = struct
  type t =
    { character : Direction.t Sprite_sheet.t
    ; grass : Draw_actions.Loadable_texture.t
    }

  let load ~character_file ~grass_file =
    let character =
      Raylib.load_image character_file
      |> Sprite_sheet.load_exn
           ~framecounts:`Auto_detect
           ~width:64
           ~height:64
           ~row_selector:Direction.Variants.to_rank
           ~row_equal:Direction.equal
    in
    { character
    ; grass = Draw_actions.Loadable_texture.create (Raylib.load_image grass_file)
    }
  ;;
end

module Character_state = struct
  type t =
    { facing : Direction.t
    ; moving : bool
    }
  [@@deriving sexp, equal]
end

let read_character_state () =
  let%sub state, set_state =
    Bonsai.state (module Character_state) ~default_model:{ facing = Down; moving = false }
  in
  let%sub () =
    Edge.after_display
      (let%map set_state = set_state
       and state = state in
       set_state { state with moving = false })
  in
  let set_moving key facing =
    Engine.Registered_events.key_down
      (Value.return key)
      ~f:
        (let%map set_state = set_state in
         fun () -> set_state { facing; moving = true })
  in
  let%sub () = set_moving W Up in
  let%sub () = set_moving A Left in
  let%sub () = set_moving S Down in
  let%sub () = set_moving D Right in
  return state
;;

let run sprite_files =
  let%sub state = read_character_state () in
  let%sub position, set_position =
    Bonsai.state (module Draw_actions.Vector2) ~default_model:{ x = 0.; y = 0. }
  in
  let%sub () =
    Edge.after_display
      (let%map state = state
       and position = position
       and set_position = set_position in
       if state.moving
       then
         set_position
           Draw_actions.Vector2.O.(position + (Direction.unit_vector state.facing * 2.))
       else Ui_effect.Ignore)
  in
  let%sub draw_action =
    Sprite_sheet.create
      sprite_files.Sprite_files.character
      ~target:
        (let%map position = position in
         `Position position)
      (let%map state = state in
       { Sprite_sheet.State.selected_row = state.facing
       ; current_frame = (if state.moving then `Cycle_every_n_frames 8 else `Constant 0)
       })
  in
  let camera =
    let%map position = position in
    Sprite_sheet.target_of_position sprite_files.character ~position
    |> Draw_actions.Camera2D.center ~canvas_width:width ~canvas_height:height
  in
  let grass =
    (* TODO this should be a repeat thing *)
    Sequence.to_list_rev
    @@
    let%bind.Sequence x = Sequence.range (-100) 100 in
    let%map.Sequence y = Sequence.range (-100) 100 in
    Draw_actions.Instructions.simple_texture
      sprite_files.grass
      ~top_left:{ x = Float.of_int (x * 64); y = Float.of_int (y * 64) }
  in
  let%arr draw_action = draw_action
  and camera = camera in
  { Draw_actions.instructions = T (Mode_2d (camera, Many [ Many grass; draw_action ]))
  ; background_color = Raylib.Color.white
  }
;;

let config : Engine.Config.t =
  { target_fps = 60; width; height; config_flags = []; title = "Raylib RPG game" }
;;

let run ~character_file ~grass_file =
  Sprite_files.load ~character_file ~grass_file |> run |> Engine.run config
;;
