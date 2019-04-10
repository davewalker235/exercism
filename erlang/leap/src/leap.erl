-module(leap).

-export([leap_year/1]).

% Use short circuit operators to ensure math is only calculated when necessary
leap_year(Year) ->
  Year rem 4 =:= 0 andalso (Year rem 100 =/= 0 orelse Year rem 400 =:= 0).
