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

let mouse_wheel_move =
  make_input
    (module Float)
    ~default_model:0.
    ~get:(Bonsai.Value.return Raylib.get_mouse_wheel_move)
;;

let frame_time =
  let get () = Raylib.get_frame_time () |> Time_ns.Span.of_sec in
  make_input
    (module Time_ns.Span)
    ~default_model:Time_ns.Span.zero
    ~get:(Bonsai.Value.return get)
;;

let fps =
  make_input (module Int) ~default_model:0 ~get:(Bonsai.Value.return Raylib.get_fps)
;;
