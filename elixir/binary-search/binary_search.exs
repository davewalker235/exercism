defmodule BinarySearch do
  @doc """
    Searches for a key in the tuple using the binary search algorithm.
    It returns :not_found if the key is not in the tuple.
    Otherwise returns {:ok, index}.

    ## Examples

      iex> BinarySearch.search({}, 2)
      :not_found

      iex> BinarySearch.search({1, 3, 5}, 2)
      :not_found

      iex> BinarySearch.search({1, 3, 5}, 5)
      {:ok, 2}

  """

  @spec search(tuple, integer) :: {:ok, integer} | :not_found
  def search({}, _key) do
    :not_found
  end
  def search(numbers, key) do
    size = tuple_size(numbers) - 1
    search(numbers, key, 0, size)
  end
  def search(numbers, key, index_start, index_end) when index_start === index_end do
    :not_found
  end
  def search(numbers, key, index_start, index_end) do
    curr_index = index_start - 1 + div(index_end - index_start, 2)
    IO.inspect {numbers, key, index_start, index_end, curr_index}
    if key > elem(numbers, index_end) do
      :not_found
    else
      case elem(numbers, curr_index) do
        i when i === key -> {:ok, curr_index}
        i when i > key -> search(numbers, key, index_start, curr_index)
        i when i < key -> search(numbers, key, curr_index, index_end)
      end
    end
  end
end
