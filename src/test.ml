(* TESTING COMMENT

   What we tested: Board Command State Bot

   What we omitted: We omiited testing Main and Practice as that
   functionality is demonstrated through the other compilation units.

   Our test suite demonstrates the correctness of your system: We tested
   all the individual components of our main function through testing
   each compilation unit that it uses: Board, Command, State, Bot. We
   test each function documented in the mli and all it's functionality.
   We omitted testing game play in compilation units like Main and
   Practice because game play is so incredibly varied that we cannot
   test each possible solution only overall play. *)

open OUnit2
open Board
open Command
open State
open Bot

let b = Board.from_json (Yojson.Basic.from_file "jsons/board1.json")

let ocamlregex = ".*1996.*"

let s = State.init_state b "1" true 'a' 'z'

let get_state result =
  match result with Valid s2 -> s2 | Invalid -> failwith "no state"

let get_a result =
  match result with Correct s2 -> s2 | Incorrect st -> st

let s2_pick = State.pick s "Cornell" 1000 b

let s2_pick2 = State.pick s "Food" 200 b

let s2_answer = State.answer_clue (get_state s2_pick) "80" b

let s2_answer_Food200 =
  State.answer_clue (get_state s2_pick) "Spaghetti" b

let s2_answer_Team600 =
  State.answer_clue (get_state s2_pick) "Upson Hall" b

let s2_inc_answer1 = State.answer_clue (get_state s2_pick) "1000" b

let s2_inc_answer2 = State.answer_clue (get_state s2_pick) "Cornelll" b

(* Tests for the Board module here *)

let get_answer (name : string) t c v (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output (Board.get_answer t c v)

let get_square (name : string) t c v (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Board.get_square t c v)
    ~printer:String.escaped

let get_value (name : string) t c s (expected_output : int) : test =
  name >:: fun _ -> assert_equal expected_output (Board.get_value t c s)

let get_clue (name : string) t c v (expected_output : string) : test =
  name >:: fun _ -> assert_equal expected_output (Board.get_clue t c v)

let get_category_ids (name : string) t (expected_output : string list) :
    test =
  name >:: fun _ ->
  assert_equal expected_output (Board.get_category_ids t)

let get_square_ids (name : string) t c (expected_output : string list) :
    test =
  name >:: fun _ ->
  assert_equal expected_output (Board.get_square_ids t c)

(*new tests*)
let get_categories (name : string) t (expected_output : category list) :
    test =
  name >:: fun _ ->
  assert_equal expected_output (Board.get_categories t)

let get_board_name (name : string) t (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output (Board.get_board_name t)

let get_final (name : string) t (expected_output : final) : test =
  name >:: fun _ -> assert_equal expected_output (Board.get_final t)

let get_squares
    (name : string)
    t
    cat
    (expected_output : question_square list) : test =
  name >:: fun _ ->
  assert_equal expected_output (Board.get_squares t cat)

let find_category (name : string) cats cat (expected_output : category)
    : test =
  name >:: fun _ ->
  assert_equal expected_output (Board.find_category cats cat)

let find_clue (name : string) value questions (expected_output : string)
    : test =
  name >:: fun _ ->
  assert_equal expected_output (Board.find_clue value questions)

let get_regex
    (name : string)
    board
    cat
    value
    (expected_output : Str.regexp) : test =
  name >:: fun _ ->
  assert_equal expected_output (Board.get_regex board cat value)

let get_question_from_score
    (name : string)
    value
    questions
    (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Board.get_question_from_score value questions)

let board_test =
  [
    get_answer "get answer for Cornell 200" b "Cornell" 200 "14853";
    get_answer "get answer for OCaml 200" b "OCaml" 200 "1996";
    get_answer "get answer for Food 1000" b "Food" 1000 "Ham";
    get_answer "get answer for Sports 1000" b "Sports" 1000 "1887";
    get_answer "get answer for Sports 800" b "Sports" 800
      "Highmark Stadium";
    get_answer "get answer for OCaml 400" b "OCaml" 400 "Functional";
    get_answer "get answer for Food 400" b "Food" 400 "Plum Tree";
    get_square "get square a1" b "OCaml" 200 "a1";
    get_square "get square e5" b "Food" 1000 "e5";
    ( "get_square error, output UnknownValue" >:: fun _ ->
      assert_raises (UnknownValue 10000) (fun () ->
          Board.get_square b "Food" 10000) );
    get_value "get value Food e3" b "Food" "e3" 600;
    get_value "get value Sports d1" b "Sports" "d1" 200;
    ( "get_value error, output UnknownValue" >:: fun _ ->
      assert_raises (UnknownValue 3) (fun () ->
          Board.get_value b "Food" "f2") );
    get_clue "get clue Team 300" b "Team" 800 "Where is Odessa from?";
    get_clue "get clue Team 1000" b "Team" 1000
      "What class are Gloria and Grace taking together?";
    ( "get_clue non-exixstent category error, output UnknownCategory"
    >:: fun _ ->
      assert_raises (UnknownCategory "Odessa") (fun () ->
          Board.get_clue b "Odessa" 800) );
    ( "get_clue non-exixstent value error, output UnknownValue"
    >:: fun _ ->
      assert_raises (UnknownValue 10) (fun () ->
          Board.get_clue b "Food" 10) );
    ( "get_clue non-exixstent category and value error, output \
       UnknownCategory"
    >:: fun _ ->
      assert_raises (UnknownCategory "Odessa") (fun () ->
          Board.get_clue b "Odessa" 17) );
    get_category_ids "get_category_ids" b
      [ "OCaml"; "Cornell"; "Team"; "Sports"; "Food" ];
    (*get_category_ids "get_category_ids" b2 [ "Math Problems"; "Animals
      at Cornell"; "Places to Study"; "Covid"; "Fun Facts" ];*)
    get_square_ids "get_category_ids" b "Team"
      [ "c1"; "c2"; "c3"; "c4"; "c5" ];
    get_square_ids "get_category_ids" b "OCaml"
      [ "a1"; "a2"; "a3"; "a4"; "a5" ];
    ( "get_square non-exixstent category error, output UnknownCategory"
    >:: fun _ ->
      assert_raises (UnknownCategory "Odessa") (fun () ->
          Board.get_squares b "Odessa") );
    ( "get_square case sensitive error, output UnknownCategory"
    >:: fun _ ->
      assert_raises (UnknownCategory "food") (fun () ->
          Board.get_squares b "food") );
    ( "get_square spacing error, output UnknownCategory" >:: fun _ ->
      assert_raises (UnknownCategory " Food ") (fun () ->
          Board.get_squares b " Food ") );
    find_clue "find_clue" 800
      (Board.get_squares b "Team")
      "Where is Odessa from?";
    find_clue "find_clue" 1000
      (Board.get_squares b "Food")
      "Finish the phrase: Green Eggs and ___.";
  ]

(* Tests for the Command module here *)

let parse_test
    (name : string)
    (input : string)
    (expected_output : command) : test =
  name >:: fun _ -> assert_equal expected_output (parse input)

let command_tests =
  [
    ( "parse ' Quit ', output Malformed" >:: fun _ ->
      assert_raises MalformedCommand (fun () -> parse " Quit ") );
    ( "parse ' quit sdthjnbvfghjit ', output Malformed" >:: fun _ ->
      assert_raises MalformedCommand (fun () ->
          parse " quit sdthjnbvfghj") );
    ( "parse ' oops exc ', output Malformed" >:: fun _ ->
      assert_raises MalformedCommand (fun () -> parse " quit exc") );
    ( "parse ' score now ', output Malformed" >:: fun _ ->
      assert_raises MalformedCommand (fun () -> parse " score now") );
    ( "parse 'PICK'" >:: fun _ ->
      assert_raises MalformedCommand (fun () -> parse "PICK") );
    ( "parse '  go ', output MalformedCommand" >:: fun _ ->
      assert_raises MalformedCommand (fun () -> parse "   go ") );
    ( "parse '', output Empty" >:: fun _ ->
      assert_raises Empty (fun () -> parse "") );
    ( "parse ' ', output Empty" >:: fun _ ->
      assert_raises Empty (fun () -> parse " ") );
    ( "parse 'pi ck', output MalformedCommand" >:: fun _ ->
      assert_raises MalformedCommand (fun () -> parse "pi ck") );
    ( "parse 'PICK', output MalformedCommand" >:: fun _ ->
      assert_raises MalformedCommand (fun () -> parse "PICK") );
    ( "parse 'Score', output MalformedCommand" >:: fun _ ->
      assert_raises MalformedCommand (fun () -> parse "Score") );
    parse_test
      "parse \" answer Humpback Whale \", output Answer 'Humpback \
       Whale' "
      "answer Humpback Whale" (Answer "Humpback Whale");
    parse_test
      "parse \" pick Sports 400 \", output Pick ('Sports'; 400)"
      "pick Sports 400"
      (Pick ("Sports", 400));
    parse_test "parse \" pick Ocaml 200 \", output Pick ('OCaml'; 200)"
      "pick OCaml 200"
      (Pick ("OCaml", 200));
    parse_test "parse \" pick Ocaml 200 \", output Pick ('OCaml'; 200)"
      "pick Cornell 200"
      (Pick ("Cornell", 200));
    parse_test "parse \" answer whales \", output Answer 'whales'"
      "answer whales" (Answer "whales");
    parse_test "parse ' quit ', output Quit " " quit " Quit;
    parse_test "parse ' oops ', output Oops " " oops " Oops;
    parse_test "parse ' score ', output Score " " score " Score;
    parse_test "parse 'SCORE', output Score " " score " Score;
  ]

(* Tests for the State module here *)

let pick_test
    (name : string)
    state
    cat
    value
    board
    (expected_result : State.result) : test =
  name >:: fun _ ->
  assert_equal expected_result (State.pick state cat value board)

let fail_pick name result expected_result : test =
  name >:: fun _ -> assert_equal result expected_result

let get_state result =
  match result with
  | Valid st -> st
  | Invalid -> failwith "shouldn't happen"

let answer_test name state response board expected_result : test =
  name >:: fun _ ->
  assert_equal expected_result (State.answer_clue state response board)

let fail_ans name result expected_result : test =
  name >:: fun _ -> assert_equal result expected_result

let result state cat value board = Valid state

let answer_test name state response board expected_result : test =
  name >:: fun _ ->
  assert_equal expected_result (State.answer_clue state response board)

let fail_ans name result expected_result : test =
  name >:: fun _ -> assert_equal result expected_result

let state_tests =
  [
    pick_test "pick Cornell 1000" s "Cornell" 1000 b
      (Valid (get_state s2_pick));
    pick_test "pick Food 200" s "Food" 200 b
      (Valid (get_state s2_pick2));
    pick_test "pick Cornell 8000, output: fail" s "Cornell" 8000 b
      Invalid;
    pick_test "pick cornell 800, output: fail" s "cornell" 800 b Invalid;
    pick_test "pick CORNELL 800, output: fail" s "CORNELL" 800 b Invalid;
    pick_test "pick Odessa 1000, output: fail" s "Odessa" 1000 b Invalid;
    pick_test "pick Cornell 1000, but it's taken" (get_state s2_pick)
      "Cornell" 1000 b Invalid;
    fail_pick "pick Cornell 1000 but it's taken" s2_pick Invalid;
    answer_test "answer Cornell 1000 " s "80" b
      (Correct (get_a s2_answer));
    answer_test "answer Food 200 " s "Spaghetti" b
      (Correct (get_a s2_answer_Team600));
    answer_test "answer Food 200 " s "spaghetti" b
      (Correct (get_a s2_answer_Team600));
    answer_test "answer Team 600 " s "upson hall" b
      (Correct (get_a s2_answer_Team600));
    answer_test "answer Team 600 " s "Upson" b
      (Correct (get_a s2_answer_Team600));
    fail_ans "answer Cornell 1000" s2_inc_answer1 (Incorrect s);
  ]

let get_incorrect_test name board state (expected_result : string) :
    test =
  name >:: fun _ ->
  assert_equal expected_result (Bot.get_incorrect board state)

let bot_tests = [ get_incorrect_test "get_incorrect" b s "Answer" ]

let suite =
  "test suite for jeopardy"
  >::: List.flatten
         [ board_test; command_tests; state_tests; bot_tests ]

let _ = run_test_tt_main suite
