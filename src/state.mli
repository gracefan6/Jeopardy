(** Representation of dynamic jeopardy state.

    This module represents the state of the game as it is being played,
    such as updating the player's score and updating the available
    questions and categories after getting picked. *)

(** The abstract type of values representing the state of the game.*)
type st

(** The type representing results of changes in state by picking a
    question.*)
type result =
  | Valid of st
  | Invalid

(** The type representing results of changing the state by answering a
    question.*)
type ans_result =
  | Correct of st
  | Incorrect of st

(** The abstract type of values representing squares on the board.*)
type info = {
  category : Board.c_id;
  value : int;
  id : string;
  clue : string;
  answer : string;
  regex : Str.regexp;
  taken : bool;
}

(** Prints the board.*)
val print_board : st -> unit

(** [pick state cat value board] is [Legal of st] if the question has
    not already been answered and the board is in a state such that a
    square can be picked. In all other cases, it is [Illegal].*)
val pick : st -> Board.c_id -> Board.value -> Board.t -> result

(** [answer state response board] is [Correct of st] if the answer is
    the correct answer. In all other cases, the result is [Incorrect]. 
    An answer is correct if it fulfills the regular expression*)
val answer_clue : st -> string -> Board.t -> ans_result

(** [init_state board] is the initial state of the game on board [b].
    The game is not waiting for the user to answer a question, the
    current category, square, clue, and answer are all empty, all
    questions are unanswered, and the score is 0.*)
    (** takes in 0 if single player mode, takes in 1 if multiplayer mode 
        *)
val init_state : Board.t -> string -> bool -> char -> char -> st

(** [is_dd st] is whether it is currently a daily double round*)
val is_dd : st -> bool

(**[all_taken st] is whether the entire board has been played, 
    where all the cells have been chosen already.*)
val all_taken : st -> bool

(** [get_score_a st] is the current score of Player A in the game.*)
val get_score_a : st -> int 

(** [get_score_b st] is the current score of Player B in the game.*)
val get_score_b : st -> int 

(** [get_wagers st] is the set of current wagers associated with state [st]. *)
val get_wagers : st -> (int option * bool) * (int option * bool)

(** [p1_turn st] is true if and only if it is Player A's turn in [st].*)
val p1_turn : st -> bool

(** [p2_turn st] is true if and only if it is Player B's turn in [st].*)
val p2_turn : st -> bool

(** [check_waiting] is true if the game is currently waiting for an
    answer and false otherwise.*)
val check_waiting : st -> bool

(** [get_current_clue] is the question currently being answered. It is
   the empty string if there is no question being answered.*)
val get_current_clue : st -> string

(** [get_current_answer] is the answer of the current question. It is
   the empty string if there is no question being answered.*)
val get_current_answer : st -> string

(** [get_current_player st] is [1] if it is player A's turn, and [2] if it is 
    player B's turn.*)   
val get_current_player : st -> int

(** [get_current_id st] is the id of the current square in state [st].*)
val get_current_id : st -> string

(** [get_current_square st] is the abstract data representing the current square
     in state [st].*)
val get_current_square : st -> info option

(** [get_daily_doubles st] is the list of ids for the daily doubles in 
    state [st].*)
val get_daily_doubles : st -> Board.square_id list

(** [get_level] is the player chosen difficulty level of a single player
    current game.*)
val get_level : st -> int

(** [get_mode] is the player chosen mode of a game, either multiplayer 
    or single player.*)
val get_mode : st -> bool

(** [get_round st] is the current round of state [st].*)
val get_round : st -> int

(** [get_squares st] is the list of squares in the board associated 
    with state [st].*)
val get_squares : st -> info list

(** [wager_dd w bd st] is state of the game after the current player wagers 
   [w] points on board [bd] which is in state [st]. The result is a state. *)
val wager_dd : int -> Board.t -> st -> result

(** [set_taken id squares] is the list of squares [squares], but with the 
   "taken" field of the square matching [id] set to [true].*)
val set_taken : string -> info list -> info list



 