open! Core

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
  [@@deriving equal, variants]
end

module State = struct
  type t =
    { x_displacement : float Incr.Var.t
    ; camera_rotation : float Incr.Var.t
    ; zoom : float Incr.Var.t
    ; current_action : Actions.t Incr.Var.t
    ; facing : [ `Right | `Left ] option Incr.Var.t
    }

  let init () =
    { x_displacement = Incr.Var.create 0.
    ; camera_rotation = Incr.Var.create 0.
    ; zoom = Incr.Var.create 1.
    ; current_action = Incr.Var.create Actions.Idling
    ; facing = Incr.Var.create None
    }
  ;;

  let update_from_input
    { x_displacement; camera_rotation; zoom; current_action; facing }
    { Input.move_right; move_left; rotate_right; rotate_left; scroll_wheel; reset }
    =
    Incr.Var.set current_action Idling;
    if move_right
    then (
      Incr.Var.replace x_displacement ~f:(fun x -> x +. 2.0);
      Incr.Var.set facing (Some `Right);
      Incr.Var.set current_action Walking);
    if move_left
    then (
      Incr.Var.replace x_displacement ~f:(fun x -> x -. 2.0);
      Incr.Var.set facing (Some `Left);
      Incr.Var.set current_action Walking);
    if rotate_right
    then Incr.Var.replace camera_rotation ~f:(fun r -> r -. 1.0 |> Float.max (-40.));
    if rotate_left
    then Incr.Var.replace camera_rotation ~f:(fun r -> r +. 1.0 |> Float.min 40.);
    Incr.Var.replace zoom ~f:(fun zoom ->
      zoom +. (scroll_wheel *. 0.05) |> Float.clamp_exn ~min:0.1 ~max:3.0);
    if reset
    then (
      Incr.Var.set camera_rotation 0.;
      Incr.Var.set zoom 1.)
  ;;
end

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

let make_rendering input clock ~sprite_file =
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
  let ({ State.x_displacement; camera_rotation; zoom; current_action; facing } as state) =
    State.init ()
  in
  let buildings = generate_random_buildings () in
  let player =
    let%map.Incr x_displacement = Incr.Var.watch x_displacement in
    { Rectangle.x = 400. +. x_displacement; y = 280.0; w = 40.0; h = 40.0 }
  in
  let%map.Incr input = input
  and player = player
  and rotation = Incr.Var.watch camera_rotation
  and zoom = Incr.Var.watch zoom
  and sprite =
    Sprite_sheet.create
      sprite
      ~flip_x:
        (match%map.Incr Incr.Var.watch facing with
         | None | Some `Right -> false
         | Some `Left -> true)
      ~target:
        (let%map.Incr player = player in
         `Rectangle player)
      ~clock
      (let%map.Incr selected_row = Incr.Var.watch current_action in
       { Sprite_sheet.State.selected_row
       ; current_frame = `Cycle (Time_ns.Span.of_ms 100.)
       })
  in
  State.update_from_input state input;
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

let run sprite_file = Engine.run config input (make_rendering ~sprite_file)
