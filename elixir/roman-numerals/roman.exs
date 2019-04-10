defmodule Roman do
  alias :math, as: Math

  @roman %{
    1    => "I",
    5    => "V",
    10   => "X",
    50   => "L",
    100  => "C",
    500  => "D",
    1000 => "M",
  }

  @doc """
  Convert an integer to a list of integers representing each decimal place.
  """
  def numerals(n) do
    n
    |> expand
    |> Enum.map(&to_pairs/1)
    |> List.flatten
    |> Enum.map(&to_numeral/1)
    |> Enum.join
  end

  @doc """
  Expand an integer to a list of tuples grouped by powers of 10. Tuples follow a
  {quantity, decimal place} format.

  ## Examples

      iex> Roman.expand(1000)
      [{1, 1000}]
      iex> Roman.expand(957)
      [{9, 100}, {5, 10}, {7, 1}]
  """
  def expand(n) do
    n
    |> to_string
    |> String.graphemes
    |> Enum.reverse
    |> Enum.map(fn n -> elem(Integer.parse(n), 0) end)
    |> Enum.with_index
    |> Enum.filter(fn {n, _} -> n > 0 end)
    |> Enum.map(fn {n, d} -> {n, Kernel.trunc(Math.pow(10, d))} end)
    |> Enum.reverse
  end

  @doc """
  This function takes the raw {quantity, decimal place} pairs and adjusts them
  to Roman numeral logic. Nines and fours are converted to one minus the next
  step. Anything above five is converted to a five plus the remainder.
  """
  def to_pairs({n, d}) do
    case n do
      9 -> [{1, d}, {1, d * 10}]
      4 -> [{1, d}, {1, d * 5}]
      5 -> [{1, 5 * d}]
      n when n > 5 -> [{1, 5 * d}, {n - 5, d}]
      _ -> {n, d}
    end
  end

  @doc """
  Everything should be converted to a member of the @roman map at this point,
  simply look up the roman numeral and repeat it by the quantity
  """
  def to_numeral({n, d}) do
    String.duplicate(@roman[d], n)
  end
end
