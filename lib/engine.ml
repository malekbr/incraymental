open! Core

module Input = struct
  module T = struct
    type 'a t =
      { update : unit -> unit
      ; incr : 'a Incr.t
      }

    let return v = { update = const (); incr = Incr.return v }

    let map2 a b ~f =
      { update =
          (fun () ->
            a.update ();
            b.update ())
      ; incr = Incr.map2 a.incr b.incr ~f
      }
    ;;

    let map = `Custom (fun { update; incr } ~f -> { update; incr = Incr.map incr ~f })
  end

  module CT = struct
    include T
    include Applicative.Make_using_map2 (T)
  end

  include CT

  module Creators = struct
    let key_down key =
      let var = Incr.Var.create false in
      { update = (fun () -> Incr.Var.set var (Raylib.is_key_down key))
      ; incr = Incr.Var.watch var
      }
    ;;

    let mouse_wheel_move () =
      let var = Incr.Var.create 0.0 in
      { update = (fun () -> Incr.Var.set var (Raylib.get_mouse_wheel_move ()))
      ; incr = Incr.Var.watch var
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
  (input : _ Input.t)
  make_result
  =
  Raylib.set_config_flags config_flags;
  Raylib.init_window width height title;
  Raylib.set_target_fps target_fps;
  let clock = Incr.Clock.create ~start:(Time_ns.now ()) () in
  let observer = make_result input.incr clock |> Incr.observe in
  let rec loop () =
    if Raylib.window_should_close () |> not
    then (
      Incr.Clock.advance_clock clock ~to_:(Time_ns.now ());
      input.update ();
      Incr.stabilize ();
      Incr.Observer.value_exn observer |> Draw_actions.perform;
      loop ())
  in
  loop ()
;;
