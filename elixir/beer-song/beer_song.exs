defmodule BeerSong do
  @doc """
  Get a single verse of the beer song
  """
  @spec verse(integer) :: String.t()
  def verse(0) do
    """
    No more bottles of beer on the wall, no more bottles of beer.
    Go to the store and buy some more, 99 bottles of beer on the wall.
    """
  end
  def verse(n) when n > 0 do

    {take, current, next} = case n do
      2 -> {"one", "2 bottles", "1 bottle"}
      1 -> {"it", "1 bottle", "no more bottles"}
      n -> {"one", "#{n} bottles", "#{n - 1} bottles"}
    end

    """
    #{current} of beer on the wall, #{current} of beer.
    Take #{take} down and pass it around, #{next} of beer on the wall.
    """
  end

  @doc """
  Get the entire beer song for a given range of numbers of bottles.
  """
  @spec lyrics(Range.t()) :: String.t()
  def lyrics(range \\ 99..0) do
    range
    |> Enum.map(&verse/1)
    |> Enum.join("\n")
  end
end
