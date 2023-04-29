open! Core
open! Bonsai
open! Let_syntax
open! Game

let initial_width = 800
let initial_height = 800

let config : Engine.Config.t =
  { target_fps = 60
  ; width = initial_width
  ; height = initial_height
  ; config_flags = [ Window_resizable ]
  ; title = "Sprite tagger"
  }
;;

let min_zoom (sprite : Draw_actions.Loadable_texture.t) =
  let%sub width = Input.render_width in
  let%sub height = Input.render_height in
  let%arr width = width
  and height = height in
  let sprite_width, sprite_height =
    Draw_actions.Loadable_texture.(width sprite, height sprite)
  in
  Float.min
    (Int.to_float height /. Int.to_float sprite_height)
    (Int.to_float width /. Int.to_float sprite_width)
;;

let max_pan (sprite : Draw_actions.Loadable_texture.t) (zoom : float Value.t) =
  let%sub width = Input.render_width in
  let%sub height = Input.render_height in
  let%arr width = width
  and height = height
  and zoom = zoom in
  let sprite_width, sprite_height =
    Draw_actions.Loadable_texture.(width sprite, height sprite)
  in
  Draw_actions.Vector2.(
    of_ints ~x:sprite_width ~y:sprite_height - (of_ints ~x:width ~y:height / zoom)
    |> clamp_lo zero)
;;

let pan_zoom_action =
  let%sub mouse_wheel_move = Input.mouse_wheel_move in
  let%sub mouse_wheel_move_vector = Input.mouse_wheel_move_vector in
  let%sub alt_down = Input.is_key_down (Value.return Raylib.Key.Left_alt) in
  let%arr mouse_wheel_move = mouse_wheel_move
  and mouse_wheel_move_vector = mouse_wheel_move_vector
  and alt_down = alt_down in
  if alt_down then `Zoom mouse_wheel_move else `Pan mouse_wheel_move_vector
;;

let tile_size ~(double : Raylib.Key.t) ~(halve : Raylib.Key.t) =
  let%sub size, set_size = state (module Int) ~default_model:32 in
  let%sub should_double = Input.is_key_pressed (Value.return double) in
  let%sub should_halve = Input.is_key_pressed (Value.return halve) in
  let%sub () =
    Edge.after_display
    @@
    let%map size = size
    and set_size = set_size
    and should_double = should_double
    and should_halve = should_halve in
    let size = if should_double then size * 2 else size in
    let size = if should_halve then size / 2 else size in
    Int.max size 1 |> set_size
  in
  return size
;;

let display (sprite : Draw_actions.Loadable_texture.t) =
  let%sub position, set_position =
    state (module Draw_actions.Vector2) ~default_model:Draw_actions.Vector2.zero
  in
  let%sub zoom, set_zoom = state (module Float) ~default_model:1. in
  let%sub min_zoom = min_zoom sprite in
  let%sub pan_zoom_action = pan_zoom_action in
  let%sub max_pan = max_pan sprite zoom in
  let%sub () =
    Edge.after_display
      (let%map pan_zoom_action = pan_zoom_action
       and zoom = zoom
       and set_zoom = set_zoom
       and min_zoom = min_zoom
       and position = position
       and set_position = set_position
       and max_pan = max_pan in
       match pan_zoom_action with
       | `Pan pan ->
         set_position
           Draw_actions.Vector2.O.(
             position - (pan * 4. / zoom)
             |> Draw_actions.Vector2.clamp_exn ~min:zero ~max:max_pan)
       | `Zoom zoom_diff -> zoom +. zoom_diff |> Float.max min_zoom |> set_zoom)
  in
  let%sub tile_width = tile_size ~double:Right ~halve:Left in
  let%sub tile_height = tile_size ~double:Up ~halve:Down in
  let%sub () =
    Edge.on_change
      (module struct
        type t = int * int [@@deriving sexp, equal]
      end)
      (Value.both tile_width tile_height)
      ~callback:
        (Value.return (fun (width, height) ->
           Modifiers.set_title [%string "%{config.title}: %{width#Int}x%{height#Int}"]))
  in
  let%arr zoom = zoom
  and position = position
  and tile_width = tile_width
  and tile_height = tile_height in
  let sprite_width, sprite_height =
    Draw_actions.Loadable_texture.(width sprite, height sprite)
  in
  { Draw_actions.background_color = Raylib.Color.white
  ; instructions =
      T
        (Mode_2d
           ( { offset = Draw_actions.Vector2.zero; target = position; zoom; rotation = 0. }
           , Many
               [ Draw_actions.Instructions.simple_texture
                   ~top_left:Draw_actions.Vector2.zero
                   sprite
               ; Sequence
                   (let%map.Sequence x =
                      Sequence.range
                        0
                        (Int.round_up sprite_width ~to_multiple_of:tile_width)
                        ~stop:`inclusive
                        ~stride:tile_width
                    in
                    let x = Int.to_float x in
                    Draw_actions.Instructions.Line
                      { start = { x; y = 0. }
                      ; stop = { x; y = Int.to_float sprite_height }
                      ; thickness = None
                      ; color = Raylib.Color.black
                      })
               ; Sequence
                   (let%map.Sequence y =
                      Sequence.range
                        0
                        (Int.round_up sprite_height ~to_multiple_of:tile_height)
                        ~stop:`inclusive
                        ~stride:tile_height
                    in
                    let y = Int.to_float y in
                    Draw_actions.Instructions.Line
                      { start = { x = 0.; y }
                      ; stop = { x = Int.to_float sprite_width; y }
                      ; thickness = None
                      ; color = Raylib.Color.black
                      })
               ] ))
  }
;;

let command =
  Command.basic ~summary:"Sprite tagger"
  @@
  let%map_open.Command sprite = anon ("sprite" %: Filename_unix.arg_type) in
  fun () ->
    let image = Raylib.load_image sprite in
    Draw_actions.Loadable_texture.create image |> display |> Engine.run config
;;

let () = Command_unix.run command
