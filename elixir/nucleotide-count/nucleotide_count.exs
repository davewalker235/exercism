defmodule NucleotideCount do
  @nucleotides [?A, ?C, ?G, ?T]
  @counts @nucleotides |> Enum.map(fn k -> {k, 0} end) |> Map.new

  @doc """
  Counts individual nucleotides in a DNA strand.

  ## Examples

  iex> NucleotideCount.count('AATAA', ?A)
  4

  iex> NucleotideCount.count('AATAA', ?T)
  1
  """
  @spec count([char], char) :: non_neg_integer
  def count(strand, nucleotide) do
    strand
    |> Enum.count(fn i -> i === nucleotide end)
  end

  @doc """
  Returns a summary of counts by nucleotide.

  ## Examples

  iex> NucleotideCount.histogram('AATAA')
  %{?A => 4, ?T => 1, ?C => 0, ?G => 0}
  """
  @spec histogram([char]) :: map
  def histogram(strand) do
    strand
    |> Enum.reduce(@counts, fn (i, acc) -> Map.update!(acc, i, &(&1 + 1)) end)
  end
end
