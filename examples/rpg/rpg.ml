open! Core
open! Bonsai
open! Game
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
    ; buildings : Draw_actions.Loadable_texture.t
    }

  let symmetric_small_house_rect =
    { Draw_actions.Rectangle.h = 64.; w = 72.; x = 210.; y = 118. }
  ;;

  let load ~character_file ~grass_file ~buildings_file =
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
    ; buildings = Draw_actions.Loadable_texture.create (Raylib.load_image buildings_file)
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
  let of_key key (direction : Direction.t) =
    let%sub is_down = Input.is_key_down (Value.return key) in
    let%arr is_down = is_down in
    if is_down then Some direction else None
  in
  let%sub movement =
    Computation.all [ of_key W Up; of_key A Left; of_key S Down; of_key D Right ]
    |> Computation.map ~f:(List.find_map ~f:Fn.id)
  in
  let%sub () =
    Edge.after_display
      (let%map set_state = set_state
       and state = state
       and movement = movement in
       match movement with
       | Some facing -> set_state { facing; moving = true }
       | None -> set_state { state with moving = false })
  in
  return state
;;

let zoom () =
  let%sub zoom, set_zoom = state (module Float) ~default_model:1. in
  let%sub delta_zoom = Input.mouse_wheel_move in
  let%sub () =
    Edge.after_display
    @@
    let%map delta_zoom = delta_zoom
    and set_zoom = set_zoom
    and zoom = zoom in
    Float.log zoom +. (0.5 *. delta_zoom)
    |> Float.exp
    |> Float.clamp_exn ~min:0.1 ~max:10.
    |> set_zoom
  in
  return zoom
;;

let run sprite_files =
  let%sub state = read_character_state () in
  let%sub position, set_position =
    Bonsai.state (module Draw_actions.Vector2) ~default_model:{ x = 0.; y = 0. }
  in
  let%sub zoom = zoom () in
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
    let%map position = position
    and zoom = zoom in
    Sprite_sheet.target_of_position sprite_files.character ~position
    |> Draw_actions.Camera2D.center ~zoom ~canvas_width:width ~canvas_height:height
  in
  let grass =
    Draw_actions.Instructions.Sequence
      (let%bind.Sequence x = Sequence.range (-3200) 3200 ~stride:64 in
       let%map.Sequence y = Sequence.range (-3200) 3200 ~stride:64 in
       Draw_actions.Instructions.simple_texture
         sprite_files.grass
         ~top_left:{ x = Int.to_float x; y = Int.to_float y })
  in
  let%sub fps = Input.fps in
  let%arr draw_action = draw_action
  and camera = camera
  and fps = fps in
  let in_camera : _ Draw_actions.Instructions.t =
    Many
      [ grass
      ; Texture
          { texture = sprite_files.buildings
          ; tint = Raylib.Color.white
          ; source = Sprite_files.symmetric_small_house_rect
          ; target =
              { (Draw_actions.Rectangle.scale Sprite_files.symmetric_small_house_rect 2.) with
                x = -100.
              ; y = -100.
              }
          }
      ; draw_action
      ]
  in
  { Draw_actions.instructions =
      T
        (Many
           [ Mode_2d (camera, in_camera)
           ; Text
               { content = [%string "FPS: %{fps#Int}"]
               ; position = { x = 10.; y = 10. }
               ; font_size = 18
               ; color = Raylib.Color.yellow
               }
           ])
  ; background_color = Raylib.Color.white
  }
;;

let config : Engine.Config.t =
  { target_fps = 60; width; height; config_flags = []; title = "Raylib RPG game" }
;;

let run ~character_file ~grass_file ~buildings_file =
  Sprite_files.load ~character_file ~grass_file ~buildings_file
  |> run
  |> Engine.run config
;;

let command =
  Command.basic ~summary:"Run the rpg game"
  @@
  let%map_open.Command character_file =
    flag
      "-character-sprite"
      (required Filename_unix.arg_type)
      ~doc:"<path> path to sprite"
  and grass_file =
    flag "-grass-sprite" (required Filename_unix.arg_type) ~doc:"<path> path to sprite"
  and buildings_file =
    flag
      "-buildings-sprite"
      (required Filename_unix.arg_type)
      ~doc:"<path> path to sprite"
  in
  fun () -> run ~character_file ~grass_file ~buildings_file
;;

let () = Command_unix.run command
