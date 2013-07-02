open Base

let run () =
  let t =
    Bot.init ()
  in
  Bot.search "#functionalNinja" t
  +> List.iter (flip Bot.fav t)

let () =
  while true do
    print_endline "wakeup";
    run ();
    Unix.sleep 60
  done
