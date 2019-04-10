-module(raindrops).
-export([convert/1]).

convert(N) ->
  Names = [
    {3, "Pling"},
    {5, "Plang"},
    {7, "Plong"}
  ],
  case [Name || {X, Name} <- Names, N rem X =:= 0] of
    [] -> integer_to_list(N);
    List -> lists:concat(List)
  end.
