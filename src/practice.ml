open Yojson.Safe

let form = [ ANSITerminal.white; ANSITerminal.Background Cyan ]

(* let form = [ ANSITerminal.white; ANSITerminal.Background Cyan ]

   let rec step bd state =

   let play_game f mode level k1 k2 = let t = try Some (Board.from_json
   (Yojson.Basic.from_file f)) with Sys_error _ -> None in match t with
   | None -> print_string "file does not exist \n" | Some t ->
   State.print_board (State.init_state t level mode k1 k2); print_string
   "\n"; step t (State.init_state t level mode k1 k2)

   let main x = ANSITerminal.print_string form "\nWelcome to Jeopardy:
   Practice round! \n\n "; play_game "jsons/board1.json" true "0" 'a'
   'a'

   let lets_practice i = main i *)

let rec step bd state =
  if State.all_taken state then ()
  else (
    ANSITerminal.print_string form "Enter a command";
    print_string "\n";
    let c =
      try Some (Command.parse (read_line ())) with
      | Command.Empty -> None
      | Command.MalformedCommand -> None
    in
    match c with
    | None ->
        print_string "Illegal! \n";
        step bd state
    | Some c -> (
        match c with
        | Quit -> ()
        | Pick (c, v) -> (
            if State.check_waiting state then (
              ANSITerminal.print_string form
                "please answer the question first \n";
              print_string "\n";
              step bd state)
            else
              match State.pick state c v bd with
              | Invalid ->
                  ANSITerminal.print_string form
                    "you picked illegal square \n";
                  print_string "\n";
                  step bd state
              | Valid t ->
                  print_string (" \n" ^ State.get_current_clue t ^ " \n");
                  step bd t)
        | Answer ap -> (
            if State.check_waiting state == false then (
              ANSITerminal.print_string form "please pick a question \n";
              print_string "\n";
              step bd state)
            else
              match State.answer_clue state ap bd with
              | Correct a ->
                  ANSITerminal.print_string [ ANSITerminal.green ]
                    "correct! \n";
                  State.print_board a;
                  step bd a
              | Incorrect b ->
                  ANSITerminal.print_string [ ANSITerminal.red ]
                    "incorrect :(\n ";
                  ANSITerminal.print_string [ ANSITerminal.red ]
                    ("The correct answer was: "
                    ^ State.get_current_answer state);

                  print_string "\n";
                  State.print_board b;
                  step bd b)
        | Score ->
            Printf.printf "Your score is %d \n"
              (State.get_score_a state);
            step bd state
        | Oops -> failwith "unimplemented"
        | Setup -> failwith "unimplemented"
        | _ -> failwith "unimplemented"))

(** [play_game f] starts the board in file [f]. *)
let play_game f =
  let t =
    try Some (Board.from_json (Yojson.Basic.from_file f))
    with Sys_error _ -> None
  in
  match t with
  | None -> print_string "file does not exist \n"
  | Some t ->
      let init = State.init_state t "1" true 'a' 'a' in
      State.print_board init;
      step t init

let print_boards x =
  let rec all_boards = function
    | [] -> print_string "\n"
    | h :: t ->
        if Filename.check_suffix h ".json" then
          print_string (Filename.chop_suffix h ".json")
        else ();
        print_string "\n";
        all_boards t
  in
  all_boards (Array.to_list (Sys.readdir "jsons"))

let rec pick_board x =
  let full_name = Sys.getcwd () ^ "/jsons/" ^ x ^ ".json" in
  if Sys.file_exists full_name then x
  else (
    print_string "This board does not exist. Please try again: \n";
    pick_board (read_line ()))

[@@@ocamlformat "disable"]

let lets_practice () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to Jeopardy! This is practice mode. \n";
  print_string "\n";
  ANSITerminal.print_string form
  "Type in which board you'd like to play: ";
print_string "\n";
print_boards 2;
let board = pick_board (read_line ()) in
ANSITerminal.print_string form ("    \ To pick a question, enter \"pick\" and the category and the \
value. \n\
\ To make a wager, enter \"wager\" and your desire wager. \n\
\ To input an answer, enter \"answer\" and the answer you think \
is correct. \n\
\ To view the current scoreboard, enter \"score\". \n\
\ If any time you'd like to quit, enter \"quit\".\n\
\  ");
print_string"\n";
  play_game ("jsons/" ^ board ^ ".json")
