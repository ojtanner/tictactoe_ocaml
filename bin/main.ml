open Tictactoe

let () =
  Cli.start (Game.create ())
