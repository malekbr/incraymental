open! Core

let rpg =
  Command.basic ~summary:"Run the rpg game"
  @@
  let%map_open.Command character_file =
    flag
      "-character-sprite"
      (required Filename_unix.arg_type)
      ~doc:"<path> path to sprite"
  and grass_file =
    flag "-grass-sprite" (required Filename_unix.arg_type) ~doc:"<path> path to sprite"
  and buildings_file =
    flag
      "-buildings-sprite"
      (required Filename_unix.arg_type)
      ~doc:"<path> path to sprite"
  in
  fun () -> Game.Rpg.run ~character_file ~grass_file ~buildings_file
;;

let sample =
  Command.basic ~summary:"Run the game"
  @@
  let%map_open.Command sprite =
    flag "-sprite" (required Filename_unix.arg_type) ~doc:"<path> path to sprite"
  in
  fun () -> Game.Sample.run sprite
;;

let () = Command.group [ "rpg", rpg; "sample", sample ] ~summary:"" |> Command_unix.run
