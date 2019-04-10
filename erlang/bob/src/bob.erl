-module(bob).

-export([response/1]).

response(Statement) ->
  Statement1 = string:trim(Statement),
  Statement2 = lists:reverse(Statement1),
  respond(Statement2).

respond([]) -> "Fine. Be that way!";
respond([H | _Tail] = Statement) ->
  Is_question = H =:= $?,
  Has_words = nomatch =/= re:run(Statement, "[a-zA-Z]"),
  Is_yelling = Has_words andalso Statement =:= string:uppercase(Statement),
  case {Is_question, Is_yelling} of
    {true, true} -> "Calm down, I know what I'm doing!";
    {true, false} -> "Sure.";
    {false, false} -> "Whatever.";
    {false, true} -> "Whoa, chill out!"
  end.
