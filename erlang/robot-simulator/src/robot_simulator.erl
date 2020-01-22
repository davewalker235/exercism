-module(robot_simulator).
-behaviour(gen_statem).

-export([init/1, callback_mode/0, start_link/1, handle_event/4]).
-export([advance/1, create/0, direction/1, left/1, place/3, position/1, right/1]).

-define(COMPASS, [north, east, south, west, north]).

init(_Args) ->
  {ok, north, {0, 0}}.

callback_mode() ->
    handle_event_function.

start_link(Args) ->
  {ok, PID} = gen_statem:start_link(?MODULE, Args, []),
  PID.

handle_event(cast, {place, {Direction, Position}}, _Direction, _Position) ->
  {next_state, Direction, Position};

handle_event(cast, advance, Direction, Position) ->
  {keep_state, advance_coordinates(Direction, Position)};

handle_event(cast, Way, Direction, Position) when Way == left orelse Way == right ->
  Direction1 = turn_compass(Way, Direction, ?COMPASS),
  {next_state, Direction1, Position};

handle_event({call, From}, position, _Direction, Position) ->
  {keep_state_and_data, {reply, From, Position}};
handle_event({call, From}, direction, Direction, _Position) ->
  {keep_state_and_data, {reply, From, Direction}}.

turn_compass(left, Current, Compass) -> turn_compass(right, Current, lists:reverse(Compass));
turn_compass(_Way, Current, [Current | T]) -> hd(T);
turn_compass(Way, Current, [_ | T]) -> turn_compass(Way, Current, T).

advance_coordinates(north, {X, Y}) -> {X, Y + 1};
advance_coordinates(south, {X, Y}) -> {X, Y - 1};
advance_coordinates(east, {X, Y}) -> {X + 1, Y};
advance_coordinates(west, {X, Y}) -> {X - 1, Y}.

create() -> start_link(nil).
place(Robot, Direction, Position) -> gen_statem:cast(Robot, {place, {Direction, Position}}).
advance(Robot) -> gen_statem:cast(Robot, advance).
left(Robot) -> gen_statem:cast(Robot, left).
right(Robot) -> gen_statem:cast(Robot, right).
position(Robot) -> gen_statem:call(Robot, position).
direction(Robot) -> gen_statem:call(Robot, direction).
