open! Core

module Registered_events = struct
  module Id = Unique_id.Int63 ()

  let handlers = Id.Table.create ()

  let make_event (type a) ~(enabled : (unit -> a option) Bonsai.Value.t) ~f =
    let id = Id.create () in
    let%sub.Bonsai () =
      Bonsai.Edge.on_change
        (module struct
          type t = ((unit -> a option) * (a -> unit Ui_effect.t)[@sexp.opaque])
          [@@deriving sexp]

          let equal = phys_equal
        end)
        ~callback:
          (Bonsai.Value.return (fun (enabled, f) ->
             Ui_effect.of_sync_fun
               (fun () ->
                 Hashtbl.set handlers ~key:id ~data:(fun () ->
                   match enabled () with
                   | None -> ()
                   | Some v -> f v |> Ui_effect.Expert.handle))
               ()))
        (Bonsai.Value.both enabled f)
    in
    Bonsai.Edge.lifecycle
      ~on_deactivate:
        (Bonsai.Value.return
           (Ui_effect.of_sync_fun (fun () -> Hashtbl.remove handlers id) ()))
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

module Input = struct
  module T = struct
    type 'a t =
      { update : unit -> unit
      ; value : 'a Bonsai.Value.t
      }

    let return v = { update = const (); value = Bonsai.Value.return v }

    let map2 a b ~f =
      { update =
          (fun () ->
            a.update ();
            b.update ())
      ; value = Bonsai.Value.map2 a.value b.value ~f
      }
    ;;

    let map =
      `Custom (fun { update; value } ~f -> { update; value = Bonsai.Value.map value ~f })
    ;;
  end

  module CT = struct
    include T
    include Applicative.Make_using_map2 (T)
  end

  include CT

  module Creators = struct
    let key_down key =
      let var = Bonsai.Var.create false in
      { update = (fun () -> Bonsai.Var.set var (Raylib.is_key_down key))
      ; value = Bonsai.Var.value var
      }
    ;;

    let mouse_wheel_move () =
      let var = Bonsai.Var.create 0.0 in
      { update = (fun () -> Bonsai.Var.set var (Raylib.get_mouse_wheel_move ()))
      ; value = Bonsai.Var.value var
      }
    ;;

    module Key = Raylib.Key
  end

  include Creators

  include
    Applicative.Make_let_syntax
      (CT)
      (struct
        module type S = module type of Creators
      end)
      (Creators)
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
