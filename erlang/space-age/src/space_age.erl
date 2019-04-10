-module(space_age).

-export([age/2]).

age(Planet, Seconds) -> conv(Planet, Seconds) / 31557600.

conv(earth   , Time) -> Time;
conv(mercury , Time) -> Time / 0.2408467;
conv(venus   , Time) -> Time / 0.61519726;
conv(mars    , Time) -> Time / 1.8808158;
conv(jupiter , Time) -> Time / 11.862615;
conv(saturn  , Time) -> Time / 29.447498;
conv(uranus  , Time) -> Time / 84.016846;
conv(neptune , Time) -> Time / 164.79132.
