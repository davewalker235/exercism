-module(rna_transcription).
-export([to_rna/1]).

to_rna([]) -> [];
to_rna([H | T]) ->
  lists:flatten([translate(H) | to_rna(T)]).

translate(71) -> "C"; % G
translate(67) -> "G"; % C
translate(84) -> "A"; % T
translate(65) -> "U". % A
