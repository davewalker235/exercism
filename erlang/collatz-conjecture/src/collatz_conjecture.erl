-module(collatz_conjecture).
-export([steps/1]).

steps(N) when N < 1 -> {error, "Only positive numbers are allowed"};
steps(N) -> steps(N, 0).
steps(1, C) -> C;
steps(N, C) ->
  case N rem 2 of
    0 -> steps(N div 2, C + 1);
    _ -> steps(((3 * N) + 1), C + 1)
  end.
