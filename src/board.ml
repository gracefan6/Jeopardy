open Yojson.Basic.Util

type value = int

type square_id = string

type c_id = string

type question_square = {
  id : square_id;
  question : string;
  answer : string;
  value : value;
  taken : bool;
  regex : string;
}

type category = {
  category_name : string;
  all_taken : bool;
  questions : question_square list;
}

type final = {
  question : string;
  answer : string;
  regex : string;
}

exception UnknownCategory of c_id

exception UnknownValue of value

type t = {
  board_name : string;
  categories : category list;
  final : final;
}

let question_json json =
  {
    id = json |> member "cell_id" |> to_string;
    question = json |> member "question" |> to_string;
    answer = json |> member "answer" |> to_string;
    value = json |> member "values" |> to_int;
    taken = json |> member "taken" |> to_bool;
    regex = json |> member "answer" |> to_string;
  }

let category_json json =
  {
    category_name = json |> member "id" |> to_string;
    all_taken = json |> member "taken" |> to_bool;
    questions =
      json |> member "cells" |> to_list |> List.map question_json;
  }

let final_json json =
  {
    question = json |> member "question" |> to_string;
    answer = json |> member "answer" |> to_string;
    regex = json |> member "regex" |> to_string;
  }

let from_json json =
  {
    board_name = json |> member "board_name" |> to_string;
    categories =
      json |> member "categories" |> to_list |> List.map category_json;
    final = json |> member "final" |> final_json;
  }

let get_categories t = t.categories

let get_board_name t = t.board_name

let get_final t = t.final

let get_category_ids t =
  let rec get_cats lst = function
    | h :: t -> h.category_name :: get_cats lst t
    | [] -> lst
  in
  get_cats [] t.categories

let get_squares t cat =
  let rec lookup_cat = function
    | [] -> raise (UnknownCategory cat)
    | h :: tail when h.category_name = cat -> h.questions
    | h :: tail -> lookup_cat tail
  in
  lookup_cat t.categories

let rec find_category cats cat =
  match cats with
  | [] -> raise (UnknownCategory cat)
  | h :: t -> if h.category_name = cat then h else find_category t cat

let get_square_ids t cat = List.map (fun b -> b.id) (get_squares t cat)

let rec get_question_from_score value questions =
  match questions with
  | [] -> raise (UnknownValue value)
  | h :: t ->
      if h.value = value then h.id else get_question_from_score value t

let get_square board cat value =
  try
    let c = find_category board.categories cat in
    get_question_from_score value c.questions
  with UnknownValue value -> raise (UnknownValue value)

let get_answer t category value =
  let c =
    List.find (fun x -> x.category_name = category) t.categories
  in
  let q = List.find (fun x -> x.value = value) c.questions in
  q.answer

let get_value b cat square =
  let rec lookup_square = function
    | [] -> raise (UnknownValue 3)
    | h :: t when h.id = square -> h.value
    | h :: t -> lookup_square t
  in
  lookup_square (get_squares b cat)

let rec find_clue value = function
  | [] -> raise (UnknownValue value)
  | h :: t -> if h.value = value then h.question else find_clue value t

let get_clue board cat value =
  try
    let c = find_category board.categories cat in
    find_clue value c.questions
  with
  | UnknownCategory cat -> raise (UnknownCategory cat)
  | UnknownValue value -> raise (UnknownValue value)

let get_ans board cat value =
  let rec search = function
    | [] -> raise (UnknownCategory cat)
    | h :: t -> if h.value = value then h.answer else search t
  in
  try
    let c = find_category board.categories cat in
    search c.questions
  with UnknownCategory cat -> raise (UnknownCategory cat)

let get_regex board cat value =
  let rec search = function
    | [] -> raise (UnknownCategory cat)
    | h :: t ->
        if h.value = value then Str.regexp_case_fold h.regex
        else search t
  in
  try
    let c = find_category board.categories cat in
    search c.questions
  with UnknownCategory cat -> raise (UnknownCategory cat)
