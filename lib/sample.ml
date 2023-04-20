open! Core
open! Bonsai
open Let_syntax

let width = 800
let height = 450

module Input = struct
  type t =
    { move_right : bool
    ; move_left : bool
    ; rotate_right : bool
    ; rotate_left : bool
    ; scroll_wheel : float
    ; reset : bool
    }
end

let input =
  let%map_open.Engine.Input move_right =
    Engine.Input.map2 (key_down D) (key_down Right) ~f:( || )
  and move_left = Engine.Input.map2 (key_down A) (key_down Left) ~f:( || )
  and rotate_right = key_down E
  and rotate_left = key_down Q
  and reset = key_down R
  and scroll_wheel = mouse_wheel_move () in
  { Input.move_left; move_right; rotate_right; rotate_left; scroll_wheel; reset }
;;

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
  let%sub yoinked_state = yoink state in
  let%sub () =
    Bonsai.Edge.after_display
      (let%map yoinked_state = yoinked_state
       and set_state = set_state in
       let%bind.Ui_effect state = yoinked_state in
       match state with
       | Computation_status.Active state ->
         set_state { state with current_action = Idling }
       | Inactive -> Ui_effect.Ignore)
  in
  let wrap_fn ~f =
    let%map yoinked_state = yoinked_state
    and set_state = set_state in
    fun v ->
      let%bind.Ui_effect state = yoinked_state in
      match state with
      | Computation_status.Active state ->
        (match f v state with
         | None -> Ui_effect.Ignore
         | Some state -> set_state state)
      | Inactive -> Ui_effect.Ignore
  in
  let many_keys keys ~f =
    List.map keys ~f:(fun key ->
      Engine.Registered_events.key_down
        (Value.return key)
        ~f:(wrap_fn ~f:(fun () state -> f state)))
    |> Bonsai.Computation.all_unit
  in
  let%sub () =
    many_keys [ Right; D ] ~f:(fun state ->
      Some
        { state with
          x_displacement = state.x_displacement +. 2.
        ; facing = Some `Right
        ; current_action = Walking
        })
  in
  let%sub () =
    many_keys [ Left; A ] ~f:(fun state ->
      Some
        { state with
          x_displacement = state.x_displacement -. 2.
        ; facing = Some `Left
        ; current_action = Walking
        })
  in
  let%sub () =
    many_keys [ E ] ~f:(fun state ->
      Some
        { state with camera_rotation = state.camera_rotation -. 1.0 |> Float.max (-40.) })
  in
  let%sub () =
    many_keys [ Q ] ~f:(fun state ->
      Some { state with camera_rotation = state.camera_rotation +. 1.0 |> Float.min 40. })
  in
  let%sub () =
    many_keys [ R ] ~f:(fun state -> Some { state with camera_rotation = 0.; zoom = 1. })
  in
  let%sub () =
    Engine.Registered_events.mouse_wheel_move
      ~f:
        (wrap_fn ~f:(fun scroll_wheel state ->
           Some
             { state with
               zoom =
                 state.zoom +. (scroll_wheel *. 0.05) |> Float.clamp_exn ~min:0.1 ~max:3.0
             }))
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
