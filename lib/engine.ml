open! Core

module Registered_events = struct
  let handlers = String.Table.create ()

  let make_event (type a) ~(enabled : (unit -> a option) Bonsai.Value.t) ~f =
    let%sub.Bonsai id = Bonsai.path_id in
    let%sub.Bonsai () =
      Bonsai.Edge.on_change
        (module struct
          type t = ((unit -> a option) * (a -> unit Ui_effect.t)[@sexp.opaque])
          [@@deriving sexp]

          let equal = phys_equal
        end)
        ~callback:
          (let%map.Bonsai id = id in
           fun (enabled, f) ->
             Ui_effect.of_sync_fun
               (fun () ->
                 Hashtbl.set handlers ~key:id ~data:(fun () ->
                   match enabled () with
                   | None -> ()
                   | Some v -> f v |> Ui_effect.Expert.handle))
               ())
        (Bonsai.Value.both enabled f)
    in
    Bonsai.Edge.lifecycle
      ~on_deactivate:
        (let%map.Bonsai id = id in
         Ui_effect.of_sync_fun (fun () -> Hashtbl.remove handlers id) ())
      ()
  ;;

  let key_down key ~f =
    make_event
      ~enabled:
        (let%map.Bonsai key = key in
         fun () -> Option.some_if (Raylib.is_key_down key) ())
      ~f
  ;;

  let mouse_wheel_move ~f =
    make_event
      ~enabled:
        (Bonsai.Value.return (fun () ->
           match Raylib.get_mouse_wheel_move () with
           | 0. -> None
           | v -> Some v))
      ~f
  ;;
end

module Config = struct
  type t =
    { config_flags : Raylib.ConfigFlags.t list
    ; width : int
    ; height : int
    ; title : string
    ; target_fps : int
    }
end

let run
  { Config.config_flags; width; height; title; target_fps }
  (computation : _ Bonsai.Computation.t)
  =
  Raylib.set_config_flags config_flags;
  Raylib.init_window width height title;
  Raylib.set_target_fps target_fps;
  let clock = Ui_incr.Clock.create ~start:(Time_ns.now ()) () in
  let driver = Bonsai_driver.create ~clock computation in
  let rec loop ~old =
    if Raylib.window_should_close () |> not
    then (
      Incremental.Clock.advance_clock clock ~to_:(Time_ns.now ());
      Hashtbl.iter Registered_events.handlers ~f:(fun f -> f ());
      Bonsai_driver.flush driver;
      Bonsai_driver.trigger_lifecycles driver;
      let current = Bonsai_driver.result driver in
      Draw_actions.perform current;
      Draw_actions.unload current ~old;
      loop ~old:current)
  in
  loop ~old:Draw_actions.empty
;;
