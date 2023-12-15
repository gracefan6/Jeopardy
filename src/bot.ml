type t = {
  player_id : string;
  score : int;
  turn : bool;
  level : int;
      (* 0 = easy (40 % getting right); 1 = medium (60% right); 2 = hard
         (80% right)*)
}

let create_bot n = { player_id = ""; score = 0; turn = true; level = n }

let get_incorrect board state =
  let categories = Board.get_category_ids board in
  let v = List.nth [ 200; 400; 600; 800; 1000 ] (Random.int 5) in
  let c = List.nth categories (Random.int (List.length categories)) in
  Board.get_ans board c v

let answer t board state =
  let lvl = t.level in
  let random = Random.int 10 in
  if
    (lvl = 0 && random < 4)
    || (lvl = 1 && random < 6)
    || (lvl = 2 && random < 8)
  then State.get_current_answer state
  else get_incorrect board state

(* let make_wager state score (id : string) = (* easy algorithm *) match
   [] with | [] -> failwith "huh is there just one bot" | h :: t -> if
   Human.get_score h < score then score / 25 else if Human.get_score h =
   score then score / 5 else score *)

let make_wager state =
  let score = State.get_score_b state in
  let human_score = State.get_score_a state in
  Random.self_init ();
  try
    if State.is_dd state then Random.int score |> ( + ) 5
    else Random.int score
  with _ -> Random.int 100

(* wage <= max value p_score && wage > 5 *)
(* if human_score < score then score / 25 else if human_score = score
   then score / 5 else human_score - score + 1 *)

let update_score t new_score = { t with score = new_score }
