-module(space_age).

-export([age/2]).

age(Planet, Seconds) -> Seconds / conv(Planet) / 31557600.

conv(earth  ) -> 1;
conv(mercury) -> 0.2408467;
conv(venus  ) -> 0.61519726;
conv(mars   ) -> 1.8808158;
conv(jupiter) -> 11.862615;
conv(saturn ) -> 29.447498;
conv(uranus ) -> 84.016846;
conv(neptune) -> 164.79132.
