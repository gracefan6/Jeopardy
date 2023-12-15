open Yojson.Safe

let form = [ ANSITerminal.white; ANSITerminal.Background Magenta ]

type cell = {
  cell_id : string;
  values : int;
  question : string;
  regex : string;
  answer : string;
  taken : bool;
}
[@@deriving yojson]

type category = {
  id : string;
  taken : bool;
  cells : cell list;
}
[@@deriving yojson]

type final = {
  question : string;
  answer : string;
  regex : string;
}
[@@deriving yojson]

type board = {
  board_name : string;
  categories : category list;
  final : final;
}
[@@deriving yojson]

let rec check_empty s =
  if s = "" then (
    ANSITerminal.print_string form
      "This cannot be an empty string! Please input something: ";
    print_string "\n";
    let q = read_line () in
    check_empty q)
  else s

let get_cell c i x name =
  print_string "\n";
  ANSITerminal.print_string form
    ("Enter clue #" ^ string_of_int i ^ " for category \"" ^ name
   ^ "\": ");
  print_string "\n";
  let qu = check_empty (read_line ()) in
  print_string "\n";
  ANSITerminal.print_string form
    ("Enter answer to clue #" ^ string_of_int i ^ " for category \""
   ^ name ^ "\": ");
  print_string "\n";
  let ans = check_empty (read_line ()) in

  {
    cell_id = c ^ string_of_int i;
    values = i * 200;
    question = qu;
    regex = ".*" ^ ans ^ ".*";
    answer = ans;
    taken = false;
  }

let get_cell_list c x name =
  [
    get_cell c 5 x name;
    get_cell c 4 x name;
    get_cell c 3 x name;
    get_cell c 2 x name;
    get_cell c 1 x name;
  ]

let transform str =
  match str with
  | "a" -> "first"
  | "b" -> "second"
  | "c" -> "third"
  | "d" -> "fourth"
  | "e" -> "final"
  | _ -> failwith "impossible"

let get_category str x =
  ANSITerminal.print_string form
    ("Enter the " ^ transform str ^ " category name: ");
  print_string "\n";
  let name = check_empty (read_line ()) in
  {
    id = name;
    taken = false;
    cells = List.rev (get_cell_list str x name);
  }

let get_category_list x =
  [
    get_category "e" x;
    get_category "d" x;
    get_category "c" x;
    get_category "b" x;
    get_category "a" x;
  ]

let rec check_exists s =
  if not (Sys.file_exists (Sys.getcwd () ^ "/jsons/" ^ s ^ ".json"))
  then s
  else (
    ANSITerminal.print_string form
      " Oops! Looks like this board name already exists. Try coming up \
       with another name!";
    print_string "\n";
    check_exists (check_empty (read_line ())))

let get_final x =
  ANSITerminal.print_string form
    "\n\
     Jeopardy! also has a final jeopardy round. Enter the Final \
     Jeopardy clue:  ";
  print_string "\n";
  let qu = check_empty (read_line ()) in
  ANSITerminal.print_string form "\nEnter the Final Jeopardy answer:  ";
  print_string "\n";
  let ans = check_empty (read_line ()) in
  { question = qu; answer = ans; regex = ".*" ^ ans ^ ".*" }

let main x =
  ANSITerminal.print_string form
    "\n\
     Create your own Jeopardy! board here! You can customize your own \
     categories, topics, questions, and answers to play with friends! \n\
    \ Enter the board name:  ";
  print_string "\n";
  let name = read_line () |> check_empty |> check_exists in
  ANSITerminal.print_string form
    "\n\
     A Jeopardy! board has 5 categories with 5 cells each. \n\
    \ To create a Jeopardy! board, we will first ask for the category \
     name. \n\
    \ We will then ask you to input the information for each of the 5 \
     cells within the category. \n\
    \ To create each cell on the Jeopardy! board, we will ask for you \
     to input a clue (a question to ask) and the correct answer to the \
     clue you just entered. \n\
    \ We will then repeat this process to create all five categories \
     of the board. ";
  print_string "\n";
  print_string "\n";
  let categories_list = List.rev (get_category_list x) in
  let x =
    {
      board_name = name;
      categories = categories_list;
      final = get_final x;
    }
  in
  let write_to_file = Yojson.Safe.to_file ("jsons/" ^ name ^ ".json") in
  write_to_file (board_to_yojson x)

let make_board i = main i
