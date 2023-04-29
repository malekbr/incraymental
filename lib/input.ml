open! Core
open! Bonsai.Let_syntax

let make_input (type a) (module M : Bonsai.Model with type t = a) ~default_model ~get =
  let%sub.Bonsai model, set_model = Bonsai.state (module M) ~default_model in
  let%sub.Bonsai () =
    Bonsai.Edge.after_display
      (let%map.Bonsai get = get
       and set_model = set_model in
       let%bind.Bonsai.Effect result = Ui_effect.of_sync_fun get () in
       set_model result)
  in
  Bonsai.read model
;;

let is_key_down key =
  make_input
    (module Bool)
    ~default_model:false
    ~get:
      (let%map.Bonsai key = key in
       fun () -> Raylib.is_key_down key)
;;

let is_key_pressed key =
  make_input
    (module Bool)
    ~default_model:false
    ~get:
      (let%map.Bonsai key = key in
       fun () -> Raylib.is_key_pressed key)
;;

let mouse_wheel_move =
  make_input
    (module Float)
    ~default_model:0.
    ~get:(Bonsai.Value.return Raylib.get_mouse_wheel_move)
;;

let mouse_wheel_move_vector =
  make_input
    (module Draw_actions.Vector2)
    ~default_model:Draw_actions.Vector2.zero
    ~get:
      (Bonsai.Value.return (fun () ->
         Raylib.get_mouse_wheel_move_v () |> Draw_actions.Vector2.of_raylib))
;;

let frame_time =
  let get () = Raylib.get_frame_time () |> Time_ns.Span.of_sec in
  make_input
    (module Time_ns.Span)
    ~default_model:Time_ns.Span.zero
    ~get:(Bonsai.Value.return get)
;;

let mouse_position =
  let get () = Raylib.get_mouse_position () |> Draw_actions.Vector2.of_raylib in
  make_input
    (module Draw_actions.Vector2)
    ~default_model:Draw_actions.Vector2.zero
    ~get:(Bonsai.Value.return get)
;;

let fps =
  make_input (module Int) ~default_model:0 ~get:(Bonsai.Value.return Raylib.get_fps)
;;

(* TODO might need to provide a different default model *)
let render_width =
  make_input
    (module Int)
    ~default_model:1
    ~get:(Bonsai.Value.return Raylib.get_render_width)
;;

(* TODO might need to provide a different default model *)
let render_height =
  make_input
    (module Int)
    ~default_model:1
    ~get:(Bonsai.Value.return Raylib.get_render_width)
;;
