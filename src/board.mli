(** [t] is the abstract representation of the jeopardy board *)
type t

(** [value] is the integer value of each question on the board*)
type value = int

(** [square_id] is the id of each cell on the board *)
type square_id = string

(** [c_id] is the id of each catgeory on the board*)
type c_id = string

(** [question_square] represents each cell on the board and holds the
    information within the cell*)
type question_square = {
  id : square_id;
  question : string;
  answer : string;
  value : value;
  taken : bool;
  regex : string;
}

(** [category] represents each category in the board, and holds the
    information about the questions within each category *)
type category = {
  category_name : string;
  all_taken : bool;
  questions : question_square list;
}

(** [final] represents the type of the final jeopardy question, and
    holds the question, the answer, and information about what kind of
    answers will be accepted.*)
type final = {
  question : string;
  answer : string;
  regex : string;
}

(** Raised when unknown category is encountered *)
exception UnknownCategory of c_id

(** Raised when unknown value is encountered *)
exception UnknownValue of value

(** [from_json j] is the json file of board that [j] represents.
    Requires: [j] is a valid JSON board representation. *)
val from_json : Yojson.Basic.t -> t

(** [question_json j] is a question from json file of board [j].
    Requires: [j] is a valid JSON board representation. *)
val question_json : Yojson.Basic.t -> question_square

(** [category_json j] is a category from json file of board [j].
    Requires: [j] is a valid JSON board representation. *)
val category_json : Yojson.Basic.t -> category

(** [final_json j] is a representation of onlt the questions, responses,
    and regexs from json file of board [j]. Requires: [j] is a valid
    JSON board representation. *)

val final_json : Yojson.Basic.t -> final

(* [get_categories t] gets the category from board [t] *)
val get_categories : t -> category list

(* [get_board_name t] gets the board name from board [t] *)
val get_board_name : t -> string

(** [get_category_ids t] gets the catergory ids of board [t] *)
val get_category_ids : t -> string list

(** [get_category_ids t] gets the final state of a board [t] *)
val get_final : t -> final

(** [get_square board cat value] gets the id of the cell when given a
    board, the category name, and the value*)
val get_square : t -> string -> value -> string

(** [get_squares board cat value] gets a list of question squares in a
    category [string] of a board [t]*)
val get_squares : t -> string -> question_square list

(** [get_question_from_score board cat value] gets score from a list of
    question squares [string]*)
val get_question_from_score : int -> question_square list -> string

(** [find_category cats] gets a category of category name [string] from
    a list of categories [string list]*)
val find_category : category list -> c_id -> category

(** [find_clue cats] gets a question of val [int] from a list of
    question_squares [question_square list] *)
val find_clue : int -> question_square list -> string

(** [get_clue board cat value] returns the question (aka clue) of the
    cell when given a board, category name, and value*)
val get_clue : t -> string -> value -> string

(** [get_ans board cat value] returns the answer of the cell when given
    a board, category name, and value*)
val get_ans : t -> string -> value -> string

(** [get_answer board cat value] returns the answer of the cell when
    given a board, category name, and value*)
val get_answer : t -> string -> value -> string

(** [get_value board cat square_id] returns the value of the cell when
    given a board, category name, and square_id*)
val get_value : t -> c_id -> square_id -> value

(** [get_square_ids board cat] returns all the square_ids in the given
    category as a list when given the board and the category *)
val get_square_ids : t -> c_id -> square_id list

(** [get_answer board cat value] returns the regexp of the cell when
    given a board, category name, and value*)

val get_regex : t -> string -> value -> Str.regexp
