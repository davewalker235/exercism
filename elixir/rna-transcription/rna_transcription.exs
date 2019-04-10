defmodule RNATranscription do
  @doc """
  Transcribes a character list representing DNA nucleotides to RNA. Note that
  elixir has some odd behavior around single quote char lists.  The ? notation
  forces the compiler to treat these as codepoints and avoids some of the
  confusion.

  ## Examples

  iex> RNATranscription.to_rna('ACTG')
  'UGAC'
  """
  @spec to_rna([char]) :: [char]
  def to_rna(dna) do
    dna |> Enum.map(fn c ->
      case c do
        ?G -> ?C
        ?C -> ?G
        ?T -> ?A
        ?A -> ?U
      end
    end)
  end
end
