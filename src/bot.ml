open Base
type t =
  Twitter.Oauth.t

let init () =
  MinOCamltter.get_oauth ()

let count = 100

let search str oauth =
  Twitter.Api11.Search.tweets oauth ~count str
  +> (function
     | `Ok s    -> s#statuses
     | `Error _ -> [])
  +> List.filter (fun x ->  x#retweeted_status = None)

let fav tweet oauth =
  Twitter.Api11.Favorites.create oauth tweet#id
  +> ignore
