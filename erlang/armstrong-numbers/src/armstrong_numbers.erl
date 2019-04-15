-module(armstrong_numbers).
-export([is_armstrong_number/1, num_to_list/1]).


is_armstrong_number(Number) ->
  Length = length(integer_to_list(Number)),
  Number =:= lists:sum([trunc(math:pow(X, Length)) || X <- num_to_list(Number)]).

num_to_list(Number) -> num_to_list(Number, []).
num_to_list(0, Acc) -> Acc;
num_to_list(Remainder, Acc) ->
  num_to_list(Remainder div 10, [Remainder rem 10 | Acc]).
