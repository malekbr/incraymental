open! Core

let () =
  Command_unix.run
  @@ Command.basic ~summary:"Run the game"
  @@
  let%map_open.Command sprite =
    flag "-sprite" (required Filename_unix.arg_type) ~doc:"<path> path to sprite"
  in
  fun () -> Game.Sample.run sprite
;;
