-module(accumulate).

-export([accumulate/2, test_version/0]).

accumulate(Fn, [H | T]) -> [Fn(H) | accumulate(Fn, T)];
accumulate(_, []) -> [].

test_version() -> 1.
