open Board
open State

type t

val create_bot : int -> t

(* [get_incorrect board state] *)
val get_incorrect : Board.t -> State.st -> string

(* [answer board state] *)
val answer : t -> Board.t -> State.st -> string

(* [make_wager state wager] *)
val make_wager : State.st -> int

(* [update_score bot board ] *)
val update_score : t -> int -> t
