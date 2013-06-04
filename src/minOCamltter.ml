open Ocaml_conv
open Spotlib.Spot

open Twitter
open Util
module TTS = GoogleTTS

module Consumer = Auth.Consumer

module Auth : sig

  (** Full auth information *)
  type t = { username : string; oauth : Oauth.t; } with conv(ocaml)

  val authorize : Consumer.t -> Auth.VerifiedToken.t -> t
  val authorize_interactive : string (** appname *) -> Consumer.t -> t

  val load : string -> t list
  val save : string -> t list -> unit

  (** Simple implementation for one app + one account *)
  module Single : sig
    val save : string -> t -> unit
    val load : string -> Consumer.t -> Oauth.t
    val oauth : path:string -> appname:string -> Consumer.t -> Oauth.t
  end

end = struct

  type t = { username : string; oauth : Oauth.t } with conv(ocaml)

  let authorize app (_, verif as verified_token) =
    let username, token = Auth.fetch_access_token app verified_token in
    let oauth = Auth.oauth app (token, verif) in
    { username; oauth }

  let authorize_interactive appname app =
    let url, req_resp_token = Auth.fetch_request_token app in
    print_endline & "Please grant access to " ^ appname ^ " and get a PIN at :";
    print_endline & "  " ^ url;
    print_string "Give me a PIN: "; flush stdout;
    let verif = read_line () in
    let t = authorize app (req_resp_token, verif) in
    print_endline ("Grant Success! Hello, @"^t.username^" !");
    t

  module Single = struct
    (** It forgets username and consumer *)
    let save path t =
      open_out_with path (fun ch ->
        output_string ch & String.concat "\n" [ t.oauth.Oauth.access_token;
                                                t.oauth.Oauth.access_token_secret;
                                                t.oauth.Oauth.verif;
                                                "" ])

    let load path app = open_in_with path (fun ch ->
      let token    = input_line ch in
      let secret   = input_line ch in
      let verif    = input_line ch in
      { Oauth.consumer_key  = app.Auth.Consumer.key;
        consumer_secret     = app.Auth.Consumer.secret;
        access_token        = token;
        access_token_secret = secret;
        verif               = verif })

    let oauth ~path ~appname app = try load path app with _ ->
      let t = authorize_interactive appname app in
      save path t;
      t.oauth
  end

  let load path =
    with_final
      (open_in path)
      (fun ic -> List.map t_of_ocaml_exn (Ocaml.Parser.from_channel ic))
      close_in

  let save path ts =
    with_final
      (open_out_gen [Open_wronly] 0o700 path)
      (fun oc ->
        let ppf = Format.formatter_of_out_channel oc in
        List.iter (fun t -> Ocaml.format_with ~no_poly:true ocaml_of_t ppf t) ts)
      close_out

end

let application_name = "ocamltter"
let config_file = ref "Assign a conf filename."

let cached_oauth = ref None

let get_oauth () = match !cached_oauth with
  | Some oauth -> oauth
  | None ->
      let oauth = Auth.Single.oauth ~path:!config_file ~appname:application_name Consumer.ocamltter in
      cached_oauth := Some oauth;
      oauth

