-module(strain).

-export([keep/2, discard/2]).

keep(Fn, List) ->
  map(Fn, List, true).

discard(Fn, List) ->
  map(Fn, List, false).

% Generic tail recursive function to map the fn to the list and keep the value
% if it matches the specified boolean value
map(Fn, List, Bool) ->
  map(Fn, List, Bool, []).

map(_Fn, [], _Bool, Acc) ->
  lists:reverse(Acc);
map(Fn, [H | T], Bool, Acc) ->
  Acc1 = case Fn(H) =:= Bool of
    true -> [H | Acc];
    false -> Acc
  end,
  map(Fn, T, Bool, Acc1).
