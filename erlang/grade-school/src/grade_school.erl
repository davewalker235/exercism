-module(grade_school).
-export([add/3, get/2, get/1, new/0]).

add(Name, Grade, School) ->
  School1 = [{Grade, Name} | School],
  lists:sort(fun order/2, School1).

get(Grade, School) ->
  [Name || {Grade1, Name} <- School, Grade =:= Grade1].

get(School) ->
  [Name || {_Grade, Name} <- School].

new() -> [].

order({GradeA, NameA}, {GradeB, NameB}) ->
  (GradeA > GradeB) and (NameA > NameB).
