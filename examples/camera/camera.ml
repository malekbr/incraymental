open! Core
open! Bonsai
open! Game
open Let_syntax

let width = 800
let height = 450

module Actions = struct
  type t =
    | Idling
    | Walking
    | Quick_draw
    | Falling_sword
    | Rising_sword
    | Jump
    | Distress
    | Death
  [@@deriving sexp, equal, variants]
end

module Input = struct
  type t =
    { move : [ `Right | `Left ] option
    ; rotate : [ `Right | `Left ] option
    ; wheel_move : float
    ; reset_camera : bool
    }

  let any_key keys =
    List.map keys ~f:(fun key -> Input.is_key_down (Value.return key))
    |> Computation.fold_right
         ~f:(fun a b -> Value.map2 a b ~f:( || ) |> read)
         ~init:(Value.return false)
  ;;

  let gather =
    let%sub move_right = any_key [ Right; D ] in
    let%sub move_left = any_key [ Left; A ] in
    let%sub rotate_right = any_key [ E ] in
    let%sub rotate_left = any_key [ Q ] in
    let%sub wheel_move = Input.mouse_wheel_move in
    let%sub reset_camera = any_key [ R ] in
    let%arr move_right = move_right
    and move_left = move_left
    and rotate_right = rotate_right
    and rotate_left = rotate_left
    and wheel_move = wheel_move
    and reset_camera = reset_camera in
    let direction right left =
      match right, left with
      | true, true | false, false -> None
      | true, false -> Some `Right
      | false, true -> Some `Left
    in
    { move = direction move_right move_left
    ; rotate = direction rotate_right rotate_left
    ; wheel_move
    ; reset_camera
    }
  ;;
end

module State = struct
  type t =
    { x_displacement : float
    ; camera_rotation : float
    ; zoom : float
    ; current_action : Actions.t
    ; facing : [ `Right | `Left ] option
    }
  [@@deriving sexp, equal]

  let initial =
    { x_displacement = 0.
    ; camera_rotation = 0.
    ; zoom = 1.
    ; current_action = Idling
    ; facing = None
    }
  ;;
end

let make_state () =
  let%sub state, set_state = Bonsai.state (module State) ~default_model:State.initial in
  let%sub input = Input.gather in
  let%sub () =
    Bonsai.Edge.after_display
      (let%map state = state
       and set_state = set_state
       and { move; rotate; wheel_move; reset_camera } = input in
       let state =
         match move with
         | None -> { state with current_action = Idling }
         | Some direction ->
           let state = { state with current_action = Walking; facing = Some direction } in
           (match direction with
            | `Right -> { state with x_displacement = state.x_displacement +. 2. }
            | `Left -> { state with x_displacement = state.x_displacement -. 2. })
       in
       let state =
         Option.value_map rotate ~default:state ~f:(function
           | `Right ->
             { state with
               camera_rotation = state.camera_rotation +. 1.0 |> Float.min 40.
             }
           | `Left ->
             { state with
               camera_rotation = state.camera_rotation -. 1.0 |> Float.max (-40.)
             })
       in
       let state =
         { state with
           zoom = state.zoom +. (wheel_move *. 0.05) |> Float.clamp_exn ~min:0.1 ~max:3.0
         }
       in
       let state =
         if reset_camera
         then
           { state with
             zoom = State.initial.zoom
           ; camera_rotation = State.initial.camera_rotation
           }
         else state
       in
       set_state state)
  in
  return state
;;

let generate_random_buildings () =
  let open Draw_actions in
  Sequence.unfold ~init:(-6000) ~f:(fun x ->
    let building_width = Raylib.get_random_value 50 200 in
    let building_height = Raylib.get_random_value 100 800 in
    Some
      ( Instructions.Rectangle
          { rect =
              { x = Float.of_int x
              ; y = Float.of_int height -. 130. -. Float.of_int building_height
              ; w = Float.of_int building_width
              ; h = Float.of_int building_height
              }
          ; color =
              Color.create
                (Raylib.get_random_value 200 240)
                (Raylib.get_random_value 200 240)
                (Raylib.get_random_value 200 250)
                255
          ; style = Fill
          }
      , x + building_width ))
  |> Fn.flip Sequence.take 100
  |> Sequence.to_list
;;

let make_rendering ~sprite_file =
  let open Draw_actions in
  let width = Float.of_int width in
  let height = Float.of_int height in
  let sprite =
    Sprite_sheet.load_exn
      (Raylib.load_image sprite_file)
      ~framecounts:`Auto_detect
      ~width:32
      ~height:32
      ~row_selector:Actions.Variants.to_rank
      ~row_equal:Actions.equal
  in
  let%sub { State.x_displacement; camera_rotation; zoom; current_action; facing } =
    make_state ()
  in
  let buildings = generate_random_buildings () in
  let player =
    let%map x_displacement = x_displacement in
    { Rectangle.x = 400. +. x_displacement; y = 280.0; w = 40.0; h = 40.0 }
  in
  let%sub sprite =
    Sprite_sheet.create
      sprite
      ~flip_x:
        (match%map facing with
         | None | Some `Right -> false
         | Some `Left -> true)
      ~target:
        (let%map player = player in
         `Rectangle player)
      (let%map selected_row = current_action in
       { Sprite_sheet.State.selected_row; current_frame = `Cycle_every_n_frames 5 })
  in
  let%arr player = player
  and rotation = camera_rotation
  and zoom = zoom
  and sprite = sprite in
  let camera =
    { Camera2D.offset = Vector2.create (width /. 2.0) (height /. 2.0)
    ; target = Vector2.create (player.x +. 20.0) (player.y +. 20.0)
    ; rotation
    ; zoom
    }
  in
  let instructions =
    Mode_2d
      ( camera
      , Many
          [ Rectangle
              { rect = { x = -6000.; y = 320.; w = 13000.; h = 8000. }
              ; style = Fill
              ; color = Color.darkgray
              }
          ; Many buildings
          ; sprite
          ; Line
              { start = { camera.target with y = -10. *. height }
              ; stop = { camera.target with y = 10. *. height }
              ; thickness = None
              ; color = Color.green
              }
          ] )
    :: [ Text
           { content = "SCREEN AREA"
           ; position = { x = 640.; y = 10. }
           ; font_size = 20
           ; color = Color.red
           }
       ; Rectangle
           { rect = { x = 0.; y = 0.; w = width; h = 5. }
           ; style = Fill
           ; color = Color.red
           }
       ; Rectangle
           { rect = { x = 0.; y = 5.; w = 5.; h = height -. 10. }
           ; style = Fill
           ; color = Color.red
           }
       ; Rectangle
           { rect = { x = width -. 5.; y = 5.; w = 5.; h = height -. 10. }
           ; style = Fill
           ; color = Color.red
           }
       ; Rectangle
           { rect = { x = 0.; y = height -. 5.; w = width; h = 5. }
           ; style = Fill
           ; color = Color.red
           }
       ; Rectangle
           { rect = { x = 10.; y = 10.; w = 250.; h = 113. }
           ; style = Fill
           ; color = Raylib.fade Color.skyblue 0.5
           }
       ; Rectangle
           { rect = { x = 10.; y = 10.; w = 250.; h = 113. }
           ; style = Lines { thickness = None }
           ; color = Color.blue
           }
       ; Text
           { content = "Free 2d camera controls:"
           ; position = { x = 20.; y = 20. }
           ; font_size = 10
           ; color = Color.black
           }
       ; List.mapi
           [ "- Right/Left to move Offset"
           ; "- Mouse Wheel to Zoom in-out"
           ; "- A / S to Rotate"
           ; "- R to reset Zoom and Rotation"
           ]
           ~f:(fun i content ->
           Instructions.Text
             { content
             ; position = { x = 40.; y = 40. +. (Float.of_int i *. 20.) }
             ; font_size = 10
             ; color = Color.darkgray
             })
         |> Many
       ]
    |> Instructions.Many
  in
  { Draw_actions.instructions = T instructions; background_color = Color.raywhite }
;;

let config : Engine.Config.t =
  { target_fps = 60
  ; width
  ; height
  ; config_flags = [ Window_resizable ]
  ; title = "raylib [core] example - 2d camera"
  }
;;

let run sprite_file = Engine.run config (make_rendering ~sprite_file)

let command =
  Command.basic ~summary:"Run the game"
  @@
  let%map_open.Command sprite =
    flag "-sprite" (required Filename_unix.arg_type) ~doc:"<path> path to sprite"
  in
  fun () -> run sprite
;;

let () = Command_unix.run command
