(* This file launches the game. *)

module T = ANSITerminal

let form = [ ANSITerminal.white; ANSITerminal.Background Blue ]

let rec pick_yes_no n =
  if n = "y" then true
  else if n = "n" then false
  else (
    Printf.printf "try picking again!";
    pick_yes_no (read_line ()))

let check_wagers_done st =
  match State.get_wagers st with
  | (Some a, _), (Some b, _) -> true
  | _ -> false

let rec pick_yes_or_no n =
  if n = "y" then true
  else if n = "n" then false
  else (
    Printf.printf "please select y or n: ";
    pick_yes_or_no (read_line ()))

let final_conditions bd state =
  let p2_turn = State.p2_turn state in
  let p1_turn = State.p1_turn state in
  let mode = State.get_mode state in
  let round = State.get_round state in
  let p2_string = if mode then "The bot " else "Player B" in
  if round = 2 && p1_turn && mode = false then (
    ANSITerminal.print_string form
      "Wow, nice work! It's time for Final Jeopardy. Here's how you \
       did:";
    ANSITerminal.print_string form
      ("Player A finished the first round with "
      ^ string_of_int (State.get_score_a state)
      ^ " points.");
    ANSITerminal.print_string form
      (p2_string ^ "finished the first round with with "
      ^ string_of_int (State.get_score_b state)
      ^ " points.");
    ANSITerminal.print_string form
      "Player A, enter your Final Jeopardy Wager")
  else if round = 2 && p2_turn && mode = false then (
    ANSITerminal.print_string form
      "Wow, nice work! It's time for Final Jeopardy. Here's how you \
       did:";
    ANSITerminal.print_string form
      ("Player A finished the first round with "
      ^ string_of_int (State.get_score_a state)
      ^ " points.");
    ANSITerminal.print_string form
      (p2_string ^ "finished the first round with "
      ^ string_of_int (State.get_score_b state)
      ^ " points.");
    ANSITerminal.print_string form
      "Player B, enter your Final Jeopardy Wager")
  else if check_wagers_done state then (
    ANSITerminal.print_string form
      "Good job! Here are the results of your game";
    ANSITerminal.print_string form
      ("Player A finished the first round with "
      ^ string_of_int (State.get_score_a state)
      ^ " points.");
    ANSITerminal.print_string form
      (p2_string ^ "finished with "
      ^ string_of_int (State.get_score_b state)
      ^ " points."))

let available st =
  List.filter
    (fun (a : State.info) -> a.taken = false)
    (State.get_squares st)

let rec step bd state =
  if State.all_taken state then
    ANSITerminal.print_string form
      "You've finished the game! Thanks for playing Jeopardy! with us \
       today. :)"
  else
    let waiting = State.check_waiting state in
    let p2_turn = State.p2_turn state in
    let p1_turn = State.p1_turn state in
    let mode = State.get_mode state in
    let round = State.get_round state in
    let available = available state in
    let current_square = State.get_current_square state in

    let is_dd =
      match current_square with
      | Some a ->
          List.mem
            (State.get_current_id state)
            (State.get_daily_doubles state)
      | None -> false
    in
    final_conditions bd state;
    if waiting = false && mode = true && p1_turn = true && is_dd = false
    then (
      ANSITerminal.print_string form "Please pick a question:";
      print_string "\n")
    else if waiting && mode && p2_turn then (
      ANSITerminal.print_string form "Bot, please enter your answer:";
      print_string "\n")
    else if waiting && mode && p2_turn == false && is_dd = true then
      print_string "\n"
    else if waiting && mode && p2_turn == false then (
      ANSITerminal.print_string form "Please enter your answer:";
      print_string "\n")
    else if waiting && mode = false && p2_turn && is_dd then
      print_string "\n"
    else if waiting && mode = false && p2_turn then (
      ANSITerminal.print_string form
        "Player B, please enter your answer:";
      print_string "\n")
    else if waiting && mode == false && p1_turn && is_dd then
      print_string "\n"
    else if mode && p2_turn then (
      ANSITerminal.print_string form
        "Bot's turn! Bot, please pick a question:";
      print_string "\n")
    else if mode && p2_turn == false then (
      ANSITerminal.print_string form "Please pick a question:";
      print_string "\n")
    else if mode = false && p2_turn && is_dd then print_string "\n"
    else if mode == false && p2_turn then (
      ANSITerminal.print_string form "Player B, please pick a question:";
      print_string "\n")
    else if mode == false && p2_turn == false then (
      ANSITerminal.print_string form "Player A, please pick a question:";
      print_string "\n")
    else ANSITerminal.print_string form " \n an error has ocurred \n";

    let c = pick_bot_command mode p2_turn bd waiting state in
    match c with
    | None ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          "That's not a valid command. Try again please! \n\
          \ Try adding add pick or answer before your command \n";
        step bd state
    | Some com -> (
        match com with
        | Command.Pick (c, v) -> pick_eval (c, v) waiting bd state
        | Answer ap -> answer_eval ap waiting p2_turn bd state
        | Wager wage ->
            let wage = int_of_string wage in
            wager_eval wage round waiting bd state
        | Score -> score_eval mode state bd
        | Oops -> failwith "unimplemented"
        | Quit -> exit 0
        | Setup -> setup_eval waiting bd state)

(* mode = true then its singile player *)
and pick_bot_command mode p2_turn bd turn state =
  let is_dd =
    match State.get_current_square state with
    | Some a ->
        List.mem
          (State.get_current_id state)
          (State.get_daily_doubles state)
    | None -> false
  in
  let command =
    if mode = false || (mode && p2_turn = false) then
      try Some (Command.parse (read_line ())) with
      | Command.Empty -> None
      | Command.MalformedCommand -> None
    else if
      mode && turn == false && p2_turn && State.get_round state = 1
    then (
      let categories = Board.get_category_ids bd in
      Random.self_init ();
      let v = List.nth [ 200; 400; 600; 800; 1000 ] (Random.int 5) in
      let c =
        List.nth categories (Random.int (List.length categories))
      in
      print_string ("pick " ^ c ^ " " ^ string_of_int v ^ "\n");
      Some (Pick (c, v)))
    else if
      (is_dd && State.get_wagers state |> snd |> fst = None)
      || State.get_round state = 2
    then (
      let wager = Bot.make_wager state in
      let wager = if wager = 0 then 50 else wager in
      print_string ("wager " ^ string_of_int wager);
      Some (Wager (string_of_int wager)))
    else
      let ap =
        Bot.answer (Bot.create_bot (State.get_level state)) bd state
      in
      print_string ("answer " ^ ap ^ " \n ");
      Some (Answer ap)
  in
  command

and setup_eval turn bd state =
  if turn then (
    print_string "Please answer the question first... \n";
    step bd state)
  else Make_board.make_board 2

and pick_eval (c, v) turn bd state =
  let new_st = State.pick state c v bd in
  let is_dd =
    match new_st with
    | Valid a ->
        List.mem (State.get_current_id a) (State.get_daily_doubles a)
    | Invalid -> false
  in

  if turn then (
    print_string "Please answer the question first... \n";
    step bd state)
  else
    match new_st with
    | Invalid ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          "\n\
          \ Please pick a valid clue. Remember that you cannot pick \
           clues that have already been answered! \n";
        step bd state
    | Valid t ->
        if is_dd = false then (
          print_string (" \n" ^ State.get_current_clue t ^ " \n");
          step bd t)
        else step bd t

and wager_eval wager round turn bd state =
  if turn then
    if round = 1 then (
      match State.wager_dd wager bd state with
      | Valid st ->
          ANSITerminal.print_string form
            "\n\
            \ Successfully wagered! Now it's time to answer the \
             question.\n";
          print_string (State.get_current_clue state);
          step bd st
      | Invalid ->
          ANSITerminal.print_string [ ANSITerminal.red ]
            "Please enter a valid wager...\n\n\
            \      Remember that you can only wager on Daily Doubles, \
             or on Final Jeopardy at the very end of the game. For \
             Daily Doubles, you can wage anywhere between 5 points and \
             the maximum of your current score and the point value of \
             the clue you picked. In Final Jeopardy, your wager must \
             be positive and less than or equal to your current score.";
          step bd state)
    else if round = 2 then (
      match State.wager_dd wager bd state with
      | Valid st ->
          if check_wagers_done st then (
            ANSITerminal.print_string form
              "Both players have successfully wagered! The final clue \
               is displayed below.";
            print_string "\n";
            print_string (State.get_current_clue st))
          else
            ANSITerminal.print_string form
              "Please enter the Final Jeopardy wagers.";
          step bd st
      | Invalid ->
          ANSITerminal.print_string form
            "Please enter a valid wager...\n\n\
            \      Remember that you can only wager on Daily Doubles, \
             or on Final Jeopardy at the very end of the game. For \
             Daily Doubles, you can wage anywhere between 5 points and \
             the maximum of your current score and the point value of \
             the clue you picked. In Final Jeopardy, your wager must \
             be positive and less than or equal to your current score.";
          step bd state)
    else failwith "impossible"

and answer_eval ap turn bot bd state =
  if turn == false then (
    ANSITerminal.print_string [ ANSITerminal.red ]
      "Please pick a question first! \n";
    step bd state)
  else if turn == true && bot == false then (
    print_string ("Is \"" ^ ap ^ "\" your final answer? \n");
    print_string "Press y for yes and n for no: ";
    if pick_yes_or_no (read_line ()) = false then (
      print_string "Type a new answer please! \n";
      step bd state)
    else
      match State.answer_clue state ap bd with
      | Correct a ->
          print_string "Correct!";
          State.print_board a;
          print_string "\n";
          step bd a
      | Incorrect b ->
          print_string "Incorrect :(";
          State.print_board b;
          print_string "\n";
          step bd b)
  else
    match State.answer_clue state ap bd with
    | Correct a ->
        ANSITerminal.print_string [ ANSITerminal.green ] "Correct :)";
        State.print_board a;
        print_string "\n";
        step bd a
    | Incorrect b ->
        ANSITerminal.print_string [ ANSITerminal.red ] "Incorrect :(";
        State.print_board b;
        print_string "\n";
        step bd b

and score_eval mode state bd =
  if mode then (
    Printf.printf "\nYour score is %d. \n" (State.get_score_a state);
    Printf.printf "The bot's score is %d. \n " (State.get_score_b state))
  else (
    Printf.printf "\nPlayer A's score is %d. \n"
      (State.get_score_a state);
    Printf.printf "Player B's score is %d. \n "
      (State.get_score_b state));
  print_string "\n";
  step bd state

(** [play_game f] starts the board in file [f]. *)
let rec pick_level level =
  if level = "0" || level = "1" || level = "2" then level
  else (
    ANSITerminal.print_string form "Try picking either 0, 1, or 2.";
    print_string "\n";
    let new_level = read_line () in
    pick_level new_level)

let play_game f mode level k1 k2 =
  let t =
    try Some (Board.from_json (Yojson.Basic.from_file f))
    with Sys_error _ -> None
  in
  match t with
  | None -> print_string "file does not exist \n"
  | Some t ->
      State.print_board (State.init_state t level mode k1 k2);
      print_string "\n";
      step t (State.init_state t level mode k1 k2)

let rec pick_mode n =
  if n = "s" then true
  else if n = "m" then false
  else (
    Printf.printf "try picking again!";
    pick_mode (read_line ()))

(** [main ()] prompts for the game to play, then starts it. *)
let setup_page x =
  ANSITerminal.print_string form
    "\n\
     Type \"make\" to create your own board, or else press any key to \
     continue to play the game";
  print_string "\n";
  let start = read_line () in
  if start = "make" then Make_board.make_board 2 else ()

let practice_real x =
  print_string "\n";
  let start = read_line () in
  if start = "practice" then Practice.lets_practice () else ()

let level_words x =
  match int_of_string x with
  | 0 -> "Easy"
  | 1 -> "Medium"
  | 2 -> "Hard"
  | _ -> failwith "implssible"

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

let main () =
  ANSITerminal.print_string form "\nWelcome to Jeopardy! \n ";
  setup_page 2;
  ANSITerminal.print_string form
  "\n\
   Type \"practice\" to practice jeopardy before playing against \
   someone, or else press any key to  play against a person or a \
   bot\n\n\
  \ ";
practice_real 2;
  ANSITerminal.print_string form
    "Let's play the real game now! Select the 's' key for single player (you will be playing against \
     a bot)\nor 'm' for multiplayer (you will be playing against another human!)";

  print_string "\n";
  let mode = pick_mode (read_line ()) in
  let level =
    if mode = true then
      pick_level
        (ANSITerminal.print_string form
           "Pick a bot level! (Input 0, 1, or 2) \n\
           \ 0 : Easy \n\
           \ 1 : Medium \n\
           \ 2 : Hard";
         print_string "\n";
         read_line ())
    else "0"
  in
  ANSITerminal.print_string form
    "Type in which board you'd like to play: ";
  print_string "\n";
  print_boards 2;
  let board = pick_board (read_line ()) in
  ANSITerminal.print_string form
    (" You are ready to start! \n  \n Currently playing: "
    ^ (if mode then
       "Single Player Mode against " ^ level_words level ^ " Bot"
      else "Multiplayer Player Mode")
    ^ "\n\n\n\
      \ To pick a question, enter \"pick\" and the category and the \
       value. \n\
      \ To make a wager, enter \"wager\" and your desire wager. \n\
      \ To input an answer, enter \"answer\" and the answer you think \
       is correct. \n\
      \ To view the current scoreboard, enter \"score\". \n\
      \ If any time you'd like to quit, enter \"quit\".\n\
      \ \n\
      \         ");
  print_string "\n";
  play_game ("jsons/" ^ board ^ ".json") mode level 'a' 'a'

[@@@ocamlformat "enable"]

(* Execute the game engine. *)
let () = main ()
