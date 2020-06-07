open Tictactoe

let () =
  Cli.loop (Game.create ())
