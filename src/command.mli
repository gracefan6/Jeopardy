(** Parsing of player commands. *)

(** The type [object_phrase] represents the object phrase that can be
    part of a player command. Each element of the list represents a word
    of the object phrase, where a {i word} is defined as a consecutive
    sequence of non-space characters. Thus, no element of the list
    should contain any leading, internal, or trailing spaces. The list
    is in the same order as the words in the original player command.
    For example:

    - If the player command is ["pick Sports 400"], then the object
      phrase is [("Sports"; 400)].

    An [object_phrase] is not permitted to be an empty tuple. *)
type object_phrase = string * int

(** An [phrase] is a string, which is used for the commands answer and
    wager. *)
type phrase = string

(** The type [command] represents a player command that is decomposed
    into a verb and possibly an object phrase. *)
type command =
  | Pick of object_phrase
  | Answer of phrase
  | Wager of phrase
  | Score
  | Oops
  | Quit
  | Setup

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception MalformedCommand

(** [parse str] parses a player's input into a [command], as follows.
    The first word (i.e., consecutive sequence of non-space characters)
    of [str] becomes the verb. The rest of the words, if any, become the
    object phrase. Examples:

    - [parse " pick Sports 400"] is [Pick \["Sports"; 400\]]
    - [parse "score"] is [Score].
    - [parse "oops"] is [Oops].
    - [parse "quit"] is [Quit].

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space
    characters (only ASCII character code 32; not tabs or newlines,
    etc.).

    Raises: [Empty] if [str] is the empty string or contains only
    spaces.

    Raises: [MalformedCommand] if the command is malformed. A command is
    {i malformed} if the verb is neither "pick", "quit", "score", nor
    "oops", or if the verb is "quit", "score", or "oops" and there is a
    non-empty object phrase, or if the verb is "pick" and there is an
    empty object phrase. *)
val parse : string -> command
