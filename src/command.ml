type object_phrase = string * int

type phrase = string

type command =
  | Pick of object_phrase
  | Answer of phrase
  | Wager of phrase
  | Score
  | Oops
  | Quit
  | Setup

exception Empty

exception MalformedCommand

let parse str =
  let sep lst =
    let reversed = List.rev lst in
    match reversed with
    | [] -> ("", 0)
    | h :: t -> (
        ( String.concat " " (List.rev t),
          try int_of_string h
          with Failure "int_of_string" -> raise MalformedCommand ))
  in

  if (fun s -> s = "") str then raise Empty
  else
    let list_of_commands =
      str |> String.split_on_char ' ' |> List.filter (fun s -> s <> "")
    in

    match list_of_commands with
    | [] -> raise Empty
    | h :: t -> (
        let comm = h in
        let phrase = t in

        match comm with
        | "answer" ->
            if String.concat " " phrase = "" then raise Empty
            else Answer (String.concat " " phrase)
        | "pick" ->
            if sep phrase = ("", 0) then raise Empty
            else Pick (sep phrase)
        | "wager" ->
            if sep phrase = ("", 0) then raise Empty
            else Wager (String.concat " " phrase)
        | "quit" ->
            if sep phrase <> ("", 0) then raise MalformedCommand
            else Quit
        | "oops" ->
            if sep phrase <> ("", 0) then raise MalformedCommand
            else Oops
        | "score" ->
            if sep phrase <> ("", 0) then raise MalformedCommand
            else Score
        | "setup" ->
            if sep phrase <> ("", 0) then raise MalformedCommand
            else Setup
              (* if keybind_sep phrase = ("", 'a') then raise Empty else *)
              (* Setup *)
              (* (keybind_sep phrase) *)
        | _ -> raise MalformedCommand)
