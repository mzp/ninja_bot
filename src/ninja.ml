let (+>) x f = f x

let o =
  MinOCamltter.get_oauth ()

let ()  =
  Twitter.Api11.Search.tweets o ~count:100 "#functionalNinja"
  +> (function
    | `Ok s  -> s#statuses
    | `Error _ -> failwith "error")
  +> List.filter (fun x ->  x#retweeted_status = None)
  +> List.map (fun x -> print_endline x#text; x)
  +> List.map (fun x -> x#id)
  +> List.map (Twitter.Api11.Favorites.create o)
  +> ignore
