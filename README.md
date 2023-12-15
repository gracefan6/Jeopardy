# Jeopardy

Our current vision is that we will be able to simulate a full multiplayer Jeopardy game in our
OCaml adaptation. This includes 3 different rounds in one game, an updating question board
and score board, time limits for questions, and Final/Double Jeopardy. We’ve realized that
implemented that using timers and delays uses coding knowledge out of our scope and would
impede the players if badly implemented, we’ve instead added warnings for players before they
submit answers and a practice mode where players can practice on boards by themselves.
We’ve also added a feature where players can create their own board to play on in single or
multiplayer mode.
