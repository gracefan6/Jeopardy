exception Invalid_Player

let bot_incorrect = false

type info = {
  category : Board.c_id;
  value : int;
  id : string;
  clue : string;
  answer : string;
  regex : Str.regexp;
  taken : bool;
}

type st = {
  round : int;
  mode : bool;
  (* true if single player false if multiplayer*)
  squares : info list;
  final : info;
  current_square : info option;
  dds : Board.square_id list;
  waiting : bool;
  (* human score always first, second score can be bot or human*)
  score : (int * bool) * (int * bool);
  wagers : (int option * bool) * (int option * bool);
  current_player : int;
  level : int;
  taken : Board.square_id list;
  keybinds : char list;
}

type result =
  | Valid of st
  | Invalid

type ans_result =
  | Correct of st
  | Incorrect of st

let all_taken st = if List.length st.taken = 25 then true else false

let get_squares st = st.squares

let get_wagers st = st.wagers

let get_current_square st = st.current_square

let get_daily_doubles st = st.dds

let check_waiting st = st.waiting

let get_current_category st =
  let sq = Option.get st.current_square in
  sq.category

let get_current_square_id st =
  let sq = Option.get st.current_square in
  sq.id

let update_taken lst id = lst :: id

let get_current_clue st =
  let sq = Option.get st.current_square in
  sq.clue

let get_current_answer st =
  let sq = Option.get st.current_square in
  sq.answer

let get_current_regex st =
  let sq = Option.get st.current_square in
  sq.regex

let get_current_id st =
  let sq = Option.get st.current_square in
  sq.id

let get_score_a st = match st.score with (a, b), n -> a

let get_score_b st = match st.score with m, (a, b) -> a

let get_mode st = st.mode

let get_round st = st.round

let get_current_player st = st.current_player

let get_level st = st.level

let p1_turn st = match st.score with (a, b), (c, d) -> b

let p2_turn st = match st.score with (a, b), (c, d) -> d

let is_dd st = List.mem (get_current_square_id st) st.dds

let set_daily_double infos round =
  let dd1 = List.nth infos (Random.int (List.length infos)) in
  if round = 1 then [ dd1.id ] else failwith "impossible"

let init_squares b =
  let rec set_squares acc = function
    | [] -> acc
    | h :: t ->
        let squares =
          List.map
            (fun x ->
              {
                category = h;
                id = x;
                value = Board.get_value b h x;
                taken = false;
                clue = Board.get_clue b h (Board.get_value b h x);
                answer = Board.get_ans b h (Board.get_value b h x);
                regex = Board.get_regex b h (Board.get_value b h x);
              })
            (Board.get_square_ids b h)
        in
        set_squares (squares @ acc) t
  in
  set_squares [] (Board.get_category_ids b)

let init_state b l new_mode k1 k2 =
  let final = Board.get_final b in
  {
    squares = init_squares b;
    final =
      {
        category = "none";
        value = 0;
        id = "final";
        clue = final.question;
        answer = final.answer;
        regex = Str.regexp_case_fold final.regex;
        taken = false;
      };
    round = 1;
    current_square = None;
    dds = [ "d2"; "a1" ];
    waiting = false;
    current_player = 1;
    score = ((0, true), (0, false));
    wagers = ((None, false), (None, false));
    mode = new_mode;
    level = int_of_string l;
    taken = [];
    keybinds = [ k1; k2 ];
  }

let switch_turn st =
  {
    st with
    score =
      (match st.score with
      | (a, b), (c, d) ->
          if b = false && d = true then ((a, true), (c, false))
          else if b = true && d = false then ((a, false), (c, true))
          else failwith "should never reach this state");
  }

let print_board state =
  print_string "               Jeopardy! Board";
  let rec print x = function
    | [] -> Printf.printf "\n"
    | h :: t ->
        if h.category <> x then Printf.printf "\n %15s  " h.category;
        if List.mem h.id state.taken then Printf.printf " %s " "XXX"
        else Printf.printf " %d " h.value;
        print h.category t
  in
  print "" state.squares

(** [set taken sq b] is the updated list of squares after picking a
    question.*)
let set_taken id infos =
  List.map
    (fun x -> if x.id = id then { x with taken = true } else x)
    infos

let pick_fill_info cat value board =
  {
    category = cat;
    id = Board.get_square board cat value;
    value;
    taken = false;
    clue = Board.get_clue board cat value;
    answer = Board.get_ans board cat value;
    regex = Board.get_regex board cat value;
  }

let pick state cat value board =
  let square =
    try Board.get_square board cat value with
    | Board.UnknownValue a -> "failure"
    | Board.UnknownCategory square -> "failure"
  in
  if List.mem square state.taken || square = "failure" then Invalid
  else
    Valid
      {
        squares = state.squares;
        final = state.final;
        current_square =
          (if List.mem square state.dds then begin
           print_string
             "Answer... Daily Double! Wager some points and then \
              answer the clue.";
           Some (pick_fill_info cat value board)
         end
          else Some (pick_fill_info cat value board));
        round = 1;
        current_player = state.current_player;
        waiting = true;
        (*fix this*)
        dds = state.dds;
        score = state.score;
        wagers = ((None, false), (None, false));
        mode = state.mode;
        level = state.level;
        taken = state.taken;
        keybinds = state.keybinds;
      }

(* type st = { round : int; mode : bool; (* true if single player false
   if multiplayer*) squares : info list; final : info; current_square :
   info option; dds : Board.square_id list; waiting : bool; score : (int
   * bool) * (int * bool); wagers : (int option * bool) * (int option *
   bool); current_player : int; level : int; (* human score always
   first, second score can be bot or human*) taken : Board.square_id
   list; keybinds : char list; } *)

let available st =
  List.filter (fun (a : info) -> a.taken = false) st.squares

let almost_empty st =
  match available st with [ a ] -> true | _ -> false

let answer_clue state response board =
  let sq = Option.get state.current_square in
  let sq_id = get_current_square_id state in
  let scores = state.score in
  let wagers = state.wagers in
  let wager_bonus =
    match wagers with
    | (a, b), (c, d) ->
        if b then try Option.get a with Invalid_argument a -> 0
        else if d then try Option.get c with Invalid_argument a -> 0
        else 0
  in
  let new_scores =
    match scores with
    | (a, b), (c, d) ->
        let sq = Option.get state.current_square in
        let new_score = Board.get_value board sq.category sq.id in
        if b then ((a + new_score, false), (c, true))
        else ((a, true), (c + new_score, false))
  in
  let new_scores_dd =
    match scores with
    | (a, b), (c, d) ->
        if b then ((a + wager_bonus, false), (c, true))
        else ((a, true), (c + wager_bonus, false))
  in
  if Str.string_match sq.regex response 0 then
    Correct
      {
        round = (if almost_empty state then 2 else 1);
        mode = state.mode;
        squares = state.squares;
        final = sq;
        current_square =
          (if almost_empty state then Some state.final else None);
        dds = state.dds;
        current_player = (if state.current_player = 1 then 2 else 1);
        wagers =
          ( (None, state.wagers |> fst |> snd),
            (None, state.wagers |> snd |> snd) );
        level = state.level;
        keybinds = state.keybinds;
        waiting = false;
        taken = sq_id :: state.taken;
        score =
          (if state.wagers |> fst |> snd || state.wagers |> snd |> snd
          then new_scores_dd
          else new_scores);
      }
  else
    Incorrect
      {
        round = (match available state with [ a ] -> 2 | _ -> 1);
        mode = state.mode;
        squares = state.squares;
        final = state.final;
        current_square = None;
        dds = state.dds;
        current_player = state.current_player;
        wagers =
          ( (None, state.wagers |> fst |> snd),
            (None, state.wagers |> snd |> snd) );
        level = state.level;
        keybinds = state.keybinds;
        taken = sq_id :: state.taken;
        waiting = false;
        (* current_square = List.nth state.squares 1; *)
        (*fix this*)
        score =
          (match state.score with
          | (a, b), (c, d) ->
              if b then ((a, false), (c, true))
              else ((a, true), (c, false)));
      }

let wager_final wage bd st =
  let current_score =
    match st.score with (a, b), (c, d) -> if b then a else c
  in
  if
    available st = []
    && st.round = 2 && wage >= 0 && wage <= current_score
  then
    Valid
      {
        st with
        round = 2;
        mode = st.mode;
        squares = st.squares;
        current_square = Some st.final;
        dds = st.dds;
        waiting = true;
        score =
          (match st.score with
          | (a, b), (c, d) ->
              if b = false && d = true then ((a, true), (c, false))
              else if b = true && d = false then ((a, false), (c, true))
              else failwith "should never reach this state");
        wagers =
          (if p1_turn st then
           ( (Some wage, true),
             (st.wagers |> snd |> fst, st.wagers |> snd |> snd) )
          else
            ( (st.wagers |> fst |> fst, st.wagers |> fst |> snd),
              (Some wage, true) ));
        current_player = st.current_player;
        level = st.level;
        taken = st.taken;
        keybinds = st.keybinds;
      }
  else Invalid

let wager_dd wage bd st =
  let p1_turn = match st.score with (a, b), (c, d) -> b in
  let p_score =
    if p1_turn then st.score |> fst |> fst else st.score |> snd |> fst
  in
  if is_dd st then
    let value =
      Board.get_value bd (Option.get st.current_square).category
        (Option.get st.current_square).id
    in
    if wage <= max value p_score && wage > 5 then
      Valid
        {
          round = st.round;
          mode = st.mode;
          squares = st.squares;
          final = st.final;
          current_square = st.current_square;
          dds = st.dds;
          waiting = true;
          score = st.score;
          wagers =
            (if p1_turn then
             ( (Some wage, true),
               (st.wagers |> snd |> fst, st.wagers |> snd |> snd) )
            else
              ( (st.wagers |> fst |> fst, st.wagers |> fst |> snd),
                (Some wage, true) ));
          current_player = st.current_player;
          level = st.level;
          taken = st.taken;
          keybinds = st.keybinds;
        }
    else Invalid
  else wager_final wage bd st

let answer_dd player answer st =
  let sq = Option.get st.current_square in
  let get_wage = function Some a -> a | None -> 0 in
  let current_wage =
    get_wage
      (if player = 1 then st.wagers |> fst |> fst
      else st.wagers |> snd |> fst)
  in
  let new_score_correct =
    if player = 1 then (st.score |> fst |> fst) + current_wage
    else (st.score |> snd |> fst) + current_wage
  in
  let new_score_incorrect =
    if player = 1 then (st.score |> fst |> fst) - current_wage
    else (st.score |> snd |> fst) - current_wage
  in
  {
    round = st.round;
    mode = st.mode;
    squares = st.squares;
    final = st.final;
    current_square = st.current_square;
    dds = st.dds;
    waiting = false;
    score =
      (if Str.string_match sq.regex answer 0 then
         print_endline
           ("Your wager was "
           ^ string_of_int current_wage
           ^ ", so "
           ^ string_of_int current_wage
           ^ " points were added to your score.");
       if player = 1 then
         ( (new_score_correct, st.score |> fst |> snd),
           (st.score |> snd |> fst, st.score |> snd |> snd) )
       else
         ( (st.score |> fst |> fst, st.score |> fst |> snd),
           (new_score_correct, st.score |> snd |> snd) ));
    wagers = st.wagers;
    current_player = st.current_player;
    level = st.level;
    taken = st.taken;
    keybinds = st.keybinds;
  }

let set_keybind st player_number key =
  let replace l position elem =
    List.mapi (fun i x -> if i = position then elem else x) l
  in
  if player_number = 1 then
    Valid { st with keybinds = replace st.keybinds 0 key }
  else if player_number = 2 then
    Valid { st with keybinds = replace st.keybinds 1 key }
  else Invalid
