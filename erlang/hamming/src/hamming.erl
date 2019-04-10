-module(hamming).
-export([distance/2]).

distance(A, B) -> distance(A, B, 0).

distance([], [], Count) -> Count;
distance([_HA | TA], [_HA | TB], Count) -> distance(TA, TB, Count);
distance([_HA | TA], [_HB | TB], Count) -> distance(TA, TB, Count + 1);
distance(_A, _B, _Count) ->
  {error, "left and right strands must be of equal length"}.
