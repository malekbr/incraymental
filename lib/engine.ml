open! Core

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
  let rec loop ~loaded_textures =
    if Raylib.window_should_close () |> not
    then (
      Incremental.Clock.advance_clock clock ~to_:(Time_ns.now ());
      Bonsai_driver.flush driver;
      Bonsai_driver.trigger_lifecycles driver;
      let current = Bonsai_driver.result driver in
      let still_loaded_textures =
        Draw_actions.perform
          current
          ~init:Draw_actions.Loadable_texture.Tracker.empty
          ~f:(fun loaded_textures (T t) ->
          match t with
          | Texture { texture; _ } ->
            Draw_actions.Loadable_texture.Tracker.track loaded_textures texture
          | _ -> loaded_textures)
      in
      Draw_actions.Loadable_texture.Tracker.unload
        loaded_textures
        ~not_in:still_loaded_textures;
      loop ~loaded_textures:still_loaded_textures)
  in
  loop ~loaded_textures:Draw_actions.Loadable_texture.Tracker.empty
;;
