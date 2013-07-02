(* botに必要な情報を格納する型(詳細は外部には公開しない) *)
type t

(* 初期化する *)
val init : unit -> t

(* キーワードで検索してTweetのリストを返す *)
val search : string -> t -> Twitter.Api_intf.Tweet.t list

(* 対象のTweetをふぁぼる *)
val fav : Twitter.Api_intf.Tweet.t -> t -> unit
