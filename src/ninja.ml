let (+>) x f = f x

let o =
  MinOCamltter.get_oauth ()

let ()  =
  Twitter.Api11.Search.tweets o ~count:100 "#functionalNinja"
  +> (function
    | `Ok s  -> s#statuses
    | `Error _ -> failwith "error")
  +> List.map (fun x -> x#text)
  +> List.iter print_endline
